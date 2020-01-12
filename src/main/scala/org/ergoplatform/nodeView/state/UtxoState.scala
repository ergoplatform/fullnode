package org.ergoplatform.nodeView.state

import java.io.File

import cats.Traverse
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.ValidationRules.{fbDigestIncorrect, fbOperationFailed}
import org.ergoplatform.settings.{Algos, VotingSettings}
import org.ergoplatform.utils.LoggingUtil
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core._
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation
  *
  * @param persistentProver - persistent prover that build authenticated AVL+ tree on top of utxo set
  * @param store            - storage of persistentProver that also keeps metadata
  * @param version          - current state version
  * @param constants        - constants, that do not change with state version changes
  */
class UtxoState(override val persistentProver: PersistentBatchAVLProver[Digest32, HF],
                override val version: VersionTag,
                override val store: LDBVersionedStore,
                override val constants: StateConstants)
  extends ErgoState[UtxoState]
    with TransactionValidation[ErgoTransaction]
    with UtxoStateReader
    with ScorexEncoding {

  override def rootHash: ADDigest = persistentProver.synchronized {
    persistentProver.digest
  }

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (constants.nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    constants.nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  import UtxoState.metadata

  override val maxRollbackDepth = 10

  override def rollbackTo(version: VersionTag): Try[UtxoState] = persistentProver.synchronized {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(scorex.core.versionToBytes(version)) match {
      case Some(hash) =>
        val rootHash: ADDigest = ADDigest @@ hash
        val rollbackResult = p.rollback(rootHash).map { _ =>
          new UtxoState(p, version, store, constants)
        }
        store.clean(constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       expectedDigest: ADDigest,
                                       currentStateContext: ErgoStateContext): Try[Unit] = {
    import cats.implicits._
    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap

    def checkBoxExistence(id: ErgoBox.BoxId): Try[ErgoBox] = createdOutputs
      .get(ByteArrayWrapper(id))
      .orElse(boxById(id))
      .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))

    val txProcessing = ErgoState.execTransactions(transactions, currentStateContext)(checkBoxExistence)
    if (txProcessing.isValid) {
      persistentProver.synchronized {
        val mods = ErgoState.stateChanges(transactions).operations.map(ADProofs.changeToMod)
        val resultTry = Traverse[List].sequence(mods.map(persistentProver.performOneOperation).toList).map(_ => ())
        ModifierValidator(stateContext.validationSettings)
          .validateNoFailure(fbOperationFailed, resultTry)
          .validateEquals(fbDigestIncorrect, expectedDigest, persistentProver.digest)
          .result
          .toTry
      }
    } else {
      txProcessing.toTry.map(_ => ())
    }
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = mod match {
    case fb: ErgoFullBlock =>
      persistentProver.synchronized {
        val height = fb.header.height

        log.debug(s"Trying to apply full block with header ${fb.header.encodedId} at height $height")

        val inRoot = rootHash

        val stateTry = stateContext.appendFullBlock(fb, votingSettings).flatMap { newStateContext =>
          applyTransactions(fb.blockTransactions.txs, fb.header.stateRoot, newStateContext).map { _: Unit =>
            val emissionBox = extractEmissionBox(fb)
            val meta = metadata(idToVersion(fb.id), fb.header.stateRoot, emissionBox, newStateContext)
            val proofBytes = persistentProver.generateProofAndUpdateStorage(meta)
            val proofHash = ADProofs.proofDigest(proofBytes)
            if (fb.adProofs.isEmpty) onAdProofGenerated(ADProofs(fb.header.id, proofBytes))

            if (!store.get(scorex.core.idToBytes(fb.id)).exists(w => java.util.Arrays.equals(w, fb.header.stateRoot))) {
              throw new Error("Storage kept roothash is not equal to the declared one")
            } else if (!java.util.Arrays.equals(fb.header.ADProofsRoot, proofHash)) {
              throw new Error("Calculated proofHash is not equal to the declared one")
            } else if (!java.util.Arrays.equals(fb.header.stateRoot, persistentProver.digest)) {
              throw new Error("Calculated stateRoot is not equal to the declared one")
            }
            log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
              s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState at height ${fb.header.height}")
            new UtxoState(persistentProver, idToVersion(fb.id), store, constants)
          }
        }
        stateTry.recoverWith[UtxoState] { case e =>
          log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(inRoot)}, reason: ${LoggingUtil.getReasonMsg(e)} ")
          persistentProver.rollback(inRoot)
            .ensuring(java.util.Arrays.equals(persistentProver.digest, inRoot))
          Failure(e)
        }
      }

    case h: Header =>
      log.warn("Only full-blocks are expected (before UTXO snapshot downloading implementation")
      //todo: update state context with headers (when snapshot downloading is done), so
      //todo: application of the first full block after the snapshot should have correct state context
      //todo: (in particular, "lastHeaders" field of it)
      Success(new UtxoState(persistentProver, idToVersion(h.id), this.store, constants))

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackVersions: Iterable[VersionTag] = persistentProver.synchronized {
    persistentProver.storage.rollbackVersions.map { v =>
      bytesToVersion(store.get(Algos.hash(v)).get)
    }
  }
}

object UtxoState {

  private lazy val bestVersionKey = Algos.hash("best state version")
  val EmissionBoxIdKey: Digest32 = Algos.hash("emission box id key")

  private def metadata(modId: VersionTag,
                       stateRoot: ADDigest,
                       currentEmissionBoxOpt: Option[ErgoBox],
                       context: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = {
    val modIdBytes = versionToBytes(modId)
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modIdBytes -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modIdBytes
    val bestVersion = bestVersionKey -> modIdBytes
    val eb = EmissionBoxIdKey -> currentEmissionBoxOpt.map(emissionBox => emissionBox.id).getOrElse(Array[Byte]())
    val cb = ErgoStateReader.ContextKey -> context.bytes

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, eb, cb)
  }

  def create(dir: File, constants: StateConstants): UtxoState = {
    val store = new LDBVersionedStore(dir, keepVersions = constants.keepVersions)
    val version = store.get(bestVersionKey).map(w => bytesToVersion(w))
      .getOrElse(ErgoState.genesisStateVersion)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
      val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedLDBAVLStorage[Digest32] = new VersionedLDBAVLStorage(store, np)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).get
    }
    new UtxoState(persistentProver, version, store, constants)
  }

  @SuppressWarnings(Array("OptionGet", "TryGet"))
  def fromBoxHolder(bh: BoxHolder,
                    currentEmissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    constants: StateConstants): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach { b =>
      p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess)
    }

    val store = new LDBVersionedStore(dir, keepVersions = constants.keepVersions)

    implicit val votingSettings: VotingSettings = constants.votingSettings

    val defaultStateContext = ErgoStateContext.empty(constants)
    val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedLDBAVLStorage[Digest32] = new VersionedLDBAVLStorage(store, np)(Algos.hash)
    val persistentProver = PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(ErgoState.genesisStateVersion, p.digest, currentEmissionBoxOpt, defaultStateContext),
      paranoidChecks = true
    ).get

    new UtxoState(persistentProver, ErgoState.genesisStateVersion, store, constants)

  }

}
