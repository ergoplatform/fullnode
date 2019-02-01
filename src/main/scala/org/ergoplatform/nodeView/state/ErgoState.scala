package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.group
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.ErgoSettings
import sigmastate.basics.DLogProtocol.ProveDlog
import scorex.core.transaction.state.MinimalState
import scorex.core.{VersionTag, bytesToVersion}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{IntArrayConstant, IntConstant, SigmaPropValue, Value}
import sigmastate.{Values, _}
import sigmastate.lang.Terms._
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utxo._

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
    StateChanges(toRemoveChanges, toInsertChanges)
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

  private def boxCreationHeight(box: Value[SBox.type]): Value[SInt.type] =
    SelectField(ExtractCreationInfo(box), 1).asIntValue

  /**
    * @param emission - emission curve
    * @return Genesis box that contains all the coins in the system, protected by the script,
    *         that allows to take part of them every block.
    */
  def genesisEmissionBox(emission: EmissionRules): ErgoBox = {
    val fixedRatePeriod = emission.settings.fixedRatePeriod
    val epochLength = emission.settings.epochLength
    val fixedRate = emission.settings.fixedRate
    val oneEpochReduction = emission.settings.oneEpochReduction
    val minerRewardDelay = emission.settings.minerRewardDelay

    val prop = emissionBoxProp(fixedRatePeriod, epochLength, fixedRate, oneEpochReduction, minerRewardDelay)
    ErgoBox(emission.coinsTotal, prop, ErgoHistory.EmptyHistoryHeight, Seq(), Map())
  }

  private def emissionBoxProp(fixedRatePeriod: Long,
                              epochLength: Int,
                              fixedRate: Long,
                              oneEpochReduction: Long,
                              minerRewardDelay: Int): Value[SBoolean.type] = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val epoch = Plus(IntConstant(1), Divide(Minus(Height, IntConstant(fixedRatePeriod.toInt)), IntConstant(epochLength)))
    val coinsToIssue = If(LT(Height, IntConstant(fixedRatePeriod.toInt)),
      fixedRate,
      Minus(fixedRate, Multiply(oneEpochReduction, epoch.upcastTo(SLong)))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(boxCreationHeight(rewardOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)

    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )
    AND(
      heightIncreased,
      correctMinerOutput,
      OR(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
    )
  }

  def generateGenesisUtxoState(stateDir: File,
                               constants: StateConstants,
                               settings: ErgoSettings): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val emissionBox = Some(genesisEmissionBox(constants.emission))
    val bh = BoxHolder(emissionBox.toSeq)

    UtxoState.fromBoxHolder(bh, emissionBox, stateDir, constants, settings).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      java.util.Arrays.equals(us.rootHash, constants.emission.settings.afterGenesisStateDigest) && us.version == genesisStateVersion
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.monetary.afterGenesisStateDigest),
      stateDir, StateConstants(None, settings))
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = bytesToVersion(Array.fill(32)(1: Byte))

  def readOrGenerate(settings: ErgoSettings,
                     constants: StateConstants): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, constants)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, constants, settings)
      case _ => ErgoState.generateGenesisUtxoState(dir, constants, settings)._1
    }
  }

  /**
    * Byte array value of the serialized reward output script proposition with pk being substituted
    * with given pk
    * @param delta
    * @param minerPkBytesVal - byte array val for pk to substitute in the reward script
    */
  def expectedMinerOutScriptBytesVal(delta: Int, minerPkBytesVal: Value[SByteArray]): Value[SByteArray] = {
    val genericPk = ProveDlog(group.generator)
    val genericMinerProp = rewardOutputScript(delta, genericPk)
    val genericMinerPropBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(genericMinerProp)
    val expectedGenericMinerProp = AND(
      GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))),
      genericPk
    )
    assert(genericMinerProp == expectedGenericMinerProp, s"reward output script changed, check and update constant position for substitution below")
    // first segregated constant is delta, so key is second constant
    val positions = IntArrayConstant(Array[Int](1))
    val minerPubkeySigmaProp = ProveDlog(DecodePoint(minerPkBytesVal))
    val newVals = Values.ConcreteCollection(Vector[SigmaPropValue](minerPubkeySigmaProp), SSigmaProp)
    SubstConstants(genericMinerPropBytes, positions, newVals)
  }

    /**
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScript(delta: Int, minerPk: ProveDlog): Value[SBoolean.type] = {
    AND(
      GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))),
      minerPk
    )
  }

  /**
    * Proposition, that allows to send coins to a box, that is protected by the following proposition:
    * prove dlog of miners public key and height is at least `delta` blocks bigger then the current one
    *
    * TODO it is possible to use creation height instead of R4, but there is no easy access to in in a script
    */
  def feeProposition(delta: Int = 720): Value[SBoolean.type] = {
    val out = ByIndex(Outputs, IntConstant(0))
    AND(
      EQ(Height, boxCreationHeight(out)),
      EQ(ExtractScriptBytes(out), expectedMinerOutScriptBytesVal(delta, MinerPubkey)),
      EQ(SizeOf(Outputs), 1)
    )
  }

}
