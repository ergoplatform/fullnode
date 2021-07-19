package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.ErgoBox.{AdditionalRegisters, R4, TokenId}
import org.ergoplatform._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.groupElemFromBytes
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Lookup, Removal, StateChanges}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{ChainSettings, Constants, ErgoSettings}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.transaction.state.MinimalState
import scorex.core.validation.ValidationResult.Valid
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.core.{VersionTag, idToVersion}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.AtLeast
import sigmastate.Values.{ByteArrayConstant, ErgoTree, IntConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.serialization.ValueSerializer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

/**
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: MinimalState[ErgoPersistentModifier, IState]]
  extends MinimalState[ErgoPersistentModifier, IState] with ErgoStateReader {

  self: IState =>


  def closeStorage(): Unit = {
    log.warn("Closing state's store.")
    store.close()
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object ErgoState extends ScorexLogging {

  type ModifierProcessing[T <: ErgoState[T]] = PartialFunction[ErgoPersistentModifier, Try[T]]

  def stateDir(settings: ErgoSettings): File = new File(s"${settings.directory}/state")

  /**
    * @param txs - sequence of transactions
    * @return ordered sequence of operations on UTXO set from this sequence of transactions
    *         if some box was created and later spend in this sequence - it is not included in the result at all
    *         if box was first spend and created after that - it is in both toInsert and toRemove
    */
  def stateChanges(txs: Seq[ErgoTransaction]): StateChanges = {
    val (toRemove, toInsert) = boxChanges(txs)
    val toRemoveChanges = toRemove.map(id => Removal(id))
    val toInsertChanges = toInsert.map(b => Insertion(b))
    val toLookup = txs.flatMap(_.dataInputs).map(b => Lookup(b.boxId))
    StateChanges(toRemoveChanges, toInsertChanges, toLookup)
  }

  /**
    * Tries to validate and execute transactions.
    *
    * @return Result of transactions execution with total cost inside
    */
  def execTransactions(transactions: Seq[ErgoTransaction],
                       currentStateContext: ErgoStateContext)
                      (checkBoxExistence: ErgoBox.BoxId => Try[ErgoBox]): ValidationResult[Long] = {
    import cats.implicits._
    implicit val verifier: ErgoInterpreter = ErgoInterpreter(currentStateContext.currentParameters)

    @tailrec
    def execTx(txs: List[ErgoTransaction], accCostTry: ValidationResult[Long]): ValidationResult[Long] = (txs, accCostTry) match {
      case (tx :: tail, r: Valid[Long]) =>
        val boxesToSpendTry: Try[List[ErgoBox]] = tx.inputs.toList
          .map(in => checkBoxExistence(in.boxId))
          .sequence

        lazy val dataBoxesTry: Try[List[ErgoBox]] = tx.dataInputs.toList
          .map(in => checkBoxExistence(in.boxId))
          .sequence

        lazy val boxes: Try[(List[ErgoBox], List[ErgoBox])] = dataBoxesTry.flatMap(db => boxesToSpendTry.map(bs => (db, bs)))

        val vs = tx.validateStateless()
          .validateNoFailure(txBoxesToSpend, boxesToSpendTry)
          .validateNoFailure(txDataBoxes, dataBoxesTry)
          .payload[Long](r.value)
          .validateTry(boxes, e => ModifierValidator.fatal("Missed data boxes", e)) { case (_, (dataBoxes, toSpend)) =>
            tx.validateStateful(toSpend.toIndexedSeq, dataBoxes.toIndexedSeq, currentStateContext, r.value)(verifier).result
          }

        execTx(tail, vs)
      case _ =>
        accCostTry
    }

    // Skip v1 block transactions validation if corresponding setting is on
    if (currentStateContext.blockVersion == 1 &&
          currentStateContext.ergoSettings.nodeSettings.skipV1TransactionsValidation) {
      Valid(0L)
    } else {
      execTx(transactions.toList, Valid[Long](0L))
    }
  }

  /**
    * @param txs - sequence of transactions
    * @return modifications from `txs` - sequence of ids to remove, and sequence of ErgoBoxes to create.
    *         if some box was created and later spend in this sequence - it is not included in the result at all
    *         if box was first spend and created after that - it is in both toInsert and toRemove,
    *         and an error will be thrown further during tree modification
    */
  def boxChanges(txs: Seq[ErgoTransaction]): (Seq[ADKey], Seq[ErgoBox]) = {
    val toInsert: mutable.HashMap[ModifierId, ErgoBox] = mutable.HashMap.empty
    val toRemove: mutable.ArrayBuffer[(ModifierId, ADKey)] = mutable.ArrayBuffer()
    txs.foreach { tx =>
      tx.inputs.foreach { i =>
        val wrapped = bytesToId(i.boxId)
        toInsert.remove(wrapped) match {
          case None => toRemove.append((wrapped, i.boxId))
          case _ => // old value removed, do nothing
        }
      }
      tx.outputs.foreach(o => toInsert += bytesToId(o.id) -> o)
    }
    (toRemove.sortBy(_._1).map(_._2), toInsert.toSeq.sortBy(_._1).map(_._2))
  }

  private def createBox(value: Long,
                        ergoTree: ErgoTree,
                        creationHeight: Int,
                        additionalTokens: Seq[(TokenId, Long)] = Nil,
                        additionalRegisters: AdditionalRegisters = Map.empty,
                        transactionId: ModifierId = ErgoBox.allZerosModifierId,
                        boxIndex: Short = 0): ErgoBox = {
    import sigmastate.eval._
    new ErgoBox(value, ergoTree,
      CostingSigmaDslBuilder.Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)
  }

  /**
    * Genesis box that contains all coins to be collected by Ergo foundation.
    * Box is protected by the script that allows to take part of them every block
    * and proposition from R4
    */
  private def genesisFoundersBox(settings: ChainSettings): ErgoBox = {
    val emission = settings.emissionRules
    val pks = settings.foundersPubkeys
      .map(str => groupElemFromBytes(Base16.decode(str).get))
      .map(pk => SigmaPropConstant(ProveDlog(pk)))
    val protection = AtLeast(IntConstant(2), pks)
    val protectionBytes = ValueSerializer.serialize(protection)
    val value = emission.foundersCoinsTotal - EmissionRules.CoinsInOneErgo
    val prop = ErgoScriptPredef.foundationScript(settings.monetary)
    createBox(value, prop, ErgoHistory.EmptyHistoryHeight, Seq(), Map(R4 -> ByteArrayConstant(protectionBytes)))
  }

  /**
    * Genesis box that contains coins in the system to be collected by miners.
    * Box is protected by the script that allows to take part of them every block.
    */
  private def genesisEmissionBox(chainSettings: ChainSettings): ErgoBox = {
    val value = chainSettings.emissionRules.minersCoinsTotal
    val prop = chainSettings.monetary.emissionBoxProposition
    createBox(value, prop, ErgoHistory.EmptyHistoryHeight, Seq(), Map())
  }

  /**
    * Genesis box that contains proofs of no premine.
    * It is a long-living box with special bytes in registers
    */
  private def noPremineBox(chainSettings: ChainSettings): ErgoBox = {
    val proofsBytes = chainSettings.noPremineProof.map(b => ByteArrayConstant(b.getBytes("UTF-8")))
    val proofs = ErgoBox.nonMandatoryRegisters.zip(proofsBytes).toMap
    createBox(EmissionRules.CoinsInOneErgo, Constants.FalseLeaf, ErgoHistory.EmptyHistoryHeight, Seq(), proofs)
  }

  /**
    * All boxes of genesis state.
    * Emission box is always the first.
    */
  def genesisBoxes(chainSettings: ChainSettings): Seq[ErgoBox] = {
    Seq(genesisEmissionBox(chainSettings), noPremineBox(chainSettings), genesisFoundersBox(chainSettings))
  }

  def generateGenesisUtxoState(stateDir: File,
                               constants: StateConstants): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val boxes = genesisBoxes(constants.settings.chainSettings)
    val bh = BoxHolder(boxes)

    UtxoState.fromBoxHolder(bh, boxes.headOption, stateDir, constants).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      java.util.Arrays.equals(us.rootHash, constants.settings.chainSettings.genesisStateDigest) && us.version == genesisStateVersion
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.genesisStateDigest),
      stateDir, StateConstants(None, settings))
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = idToVersion(Header.GenesisParentId)

  def readOrGenerate(settings: ErgoSettings,
                     constants: StateConstants): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, constants)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, constants)
      case _ => ErgoState.generateGenesisUtxoState(dir, constants)._1
    }
  }

}
