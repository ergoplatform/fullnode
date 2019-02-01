package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

trait UtxoStateReader extends ErgoStateReader with TransactionValidation[ErgoTransaction] {

  protected implicit val hf: HF = Algos.hash

  val constants: StateConstants

  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, HF]

  /**
    * Validate transaction as if it was included at the end of the last block.
    * This validation does not guarantee that transaction will be valid in future
    * as soon as state (both UTXO set and state context) will change.
    *
    * @return transaction cost
    */
  def validateWithCost(tx: ErgoTransaction): Try[Long] = {
    tx.statelessValidity.flatMap { _ =>
      implicit val verifier = ErgoInterpreter(stateContext.currentParameters)
      tx.statefulValidity(tx.inputs.flatMap(i => boxById(i.boxId)), stateContext)
    }
  }

  override def validate(tx: ErgoTransaction): Try[Unit] = validateWithCost(tx).map(_ => Unit)

  protected[state] def extractEmissionBox(fb: ErgoFullBlock): Option[ErgoBox] = {
    emissionBoxIdOpt match {
      case Some(id) =>
        fb.blockTransactions.txs.view.reverse
          .find(_.inputs.exists(t => java.util.Arrays.equals(t.boxId, id))) match {
            case Some(tx) if tx.outputs.head.proposition == constants.genesisEmissionBox.proposition =>
              tx.outputs.headOption
            case Some(_) =>
              log.info(s"Last possible emission box consumed")
              None
            case _ =>
              log.info(s"Emission box not found in block ${fb.encodedId}")
              boxById(id)
          }
      case _ => // try to find matching emission box among transactions in block.
        fb.blockTransactions.txs.view.reverse
          .find { tx =>
            tx.outputs.headOption.exists { out =>
              out.proposition == constants.genesisEmissionBox.proposition &&
                out.value == constants.emission.remainingCoinsAfterHeight(fb.header.height)
            }
          }
          .map(_.outputs.head)
          .orElse {
            log.info("No emission box: emission should be already finished before this block")
            None
          }
    }
  }

  protected def emissionBoxIdOpt: Option[ADKey] = store.get(ByteArrayWrapper(UtxoState.EmissionBoxIdKey)) match {
    case Some(w) if w.data.nonEmpty => Some(ADKey @@ w.data)
    case _ => None
  }

  def emissionBoxOpt: Option[ErgoBox] = emissionBoxIdOpt.flatMap(boxById)

  def boxById(id: ADKey): Option[ErgoBox] = persistentProver.synchronized {
    persistentProver
      .unauthenticatedLookup(id)
      .map(ErgoBoxSerializer.parseBytes)
      .flatMap(_.toOption)
  }

  def randomBox: Option[ErgoBox] = persistentProver.synchronized {
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)
  }

  /**
    * Generate proofs for specified transactions if applied to current state
    *
    * @param txs - transactions to generate proofs
    * @return proof for specified transactions and new state digest
    */
  def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = persistentProver.synchronized {
    val rootHash = persistentProver.digest
    log.trace(s"Going to create proof for ${txs.length} transactions at root ${Algos.encode(rootHash)}")
    if (txs.isEmpty) {
      Failure(new Exception("Trying to generate proof for empty transaction sequence"))
    } else if (!storage.version.exists(t => java.util.Arrays.equals(t, rootHash))) {
      Failure(new Exception(s"Incorrect storage: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}. " +
        "Possible reason - state update is in process."))
    } else {
      persistentProver.avlProver.generateProofForOperations(ErgoState.stateChanges(txs).operations.map(ADProofs.changeToMod))
    }
  }

}
