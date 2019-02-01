package org.ergoplatform.nodeView.state

import java.io.File

import cats.Traverse
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.UtxoSnapshot
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.nodeView.state.UtxoState.ModifierProcessing
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings._
import org.ergoplatform.utils.LoggingUtil
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core._
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32

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
                override val store: Store,
                override val constants: StateConstants,
                settings: ErgoSettings)
  extends ErgoState[UtxoState]
    with TransactionValidation[ErgoTransaction]
    with UtxoStateReader {

  import UtxoState.metadata

  override val maxRollbackDepth = 10

  override def rootHash: ADDigest = persistentProver.synchronized {
    persistentProver.digest
  }

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (constants.nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    constants.nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = persistentProver.synchronized {
    val p = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(Algos.versionToBAW(version)) match {
      case Some(hash) =>
        val rootHash: ADDigest = ADDigest @@ hash.data
        val rollbackResult = p.rollback(rootHash).map { _ =>
          new UtxoState(p, version, store, constants, settings)
        }
        store.clean(constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Exception(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  /**
    * Tries to validate and execute transactions.
    * @return Result of transactions execution with total cost inside
    */
  private def execTransactionsTry(transactions: Seq[ErgoTransaction],
                                  currentStateContext: ErgoStateContext): Try[Long] = {
    import cats.implicits._
    implicit val verifier: ErgoInterpreter = ErgoInterpreter(currentStateContext.currentParameters)
    val createdOutputs = transactions.flatMap(_.outputs).map(o => (ByteArrayWrapper(o.id), o)).toMap
    val execResults: Try[List[Long]] = transactions.toList
      .map { tx =>
        tx.statelessValidity.flatMap { _ =>
          val boxesToSpendTry: Try[List[ErgoBox]] = tx.inputs.toList
            .map(_.boxId)
            .map { id =>
              createdOutputs.get(ByteArrayWrapper(id)).orElse(boxById(id))
                .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))
            }
            .sequence
          boxesToSpendTry.flatMap { boxes =>
            tx.statefulValidity(boxes.toIndexedSeq, currentStateContext)
          }
        }
      }
      .sequence
    execResults.map(_.sum)
  }

  private[state] def applyTransactions(transactions: Seq[ErgoTransaction],
                                       expectedDigest: ADDigest,
                                       currentStateContext: ErgoStateContext): Try[Unit] = {
    import cats.implicits._
    execTransactionsTry(transactions, currentStateContext) match {
      case Success(executionCost) if executionCost <= currentStateContext.currentParameters.maxBlockCost =>
        persistentProver.synchronized {
          val mods = ErgoState.stateChanges(transactions).operations.map(ADProofs.changeToMod)
          val resultTry = Traverse[List].sequence(mods.map(persistentProver.performOneOperation).toList).map(_ => ())
          if (java.util.Arrays.equals(expectedDigest, persistentProver.digest) || resultTry.isFailure) {
            resultTry
          } else {
            Failure(new Exception(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
              s"${Algos.encode(persistentProver.digest)} given"))
          }
        }
      case Success(executionCost) => Failure(new Exception(s"Transaction cost $executionCost exceeds limit"))
      case failure => failure.map(_ => ())
    }
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = processing(mod)

  private def processing: ModifierProcessing = {
    applyFullBlock orElse
      applyHeader orElse
      applySnapshot orElse
      other
  }

  private def applyFullBlock: ModifierProcessing = {
    case fb: ErgoFullBlock =>
      val height = fb.header.height

      log.info(s"Trying to apply full block with header ${fb.header.encodedId} at height $height")
      stateContext.appendFullBlock(fb, votingSettings)
        .flatMap { newStateContext =>
          persistentProver.synchronized {
            val inRoot = rootHash

            applyTransactions(fb.blockTransactions.txs, fb.header.stateRoot, newStateContext)
              .map { _ =>
                val emissionBox = extractEmissionBox(fb)
                val meta = metadata(idToVersion(fb.id), fb.header.stateRoot, emissionBox, newStateContext)
                val proofBytes = persistentProver.generateProofAndUpdateStorage(meta)
                val proofHash = ADProofs.proofDigest(proofBytes)
                if (fb.adProofs.isEmpty) onAdProofGenerated(ADProofs(fb.header.id, proofBytes))

                if (!store.get(Algos.idToBAW(fb.id)).exists(w => java.util.Arrays.equals(w.data, fb.header.stateRoot))) {
                  throw new Exception("Storage kept stateRoot is not equal to the declared one")
                } else if (!java.util.Arrays.equals(fb.header.ADProofsRoot, proofHash)) {
                  throw new Exception("Calculated proofHash is not equal to the declared one")
                } else if (!java.util.Arrays.equals(fb.header.stateRoot, persistentProver.digest)) {
                  throw new Exception("Calculated stateRoot is not equal to the declared one")
                }

                log.info(s"Valid modifier with header ${fb.header.encodedId} and emission box " +
                  s"${emissionBox.map(e => Algos.encode(e.id))} applied to UtxoState with root hash ${Algos.encode(inRoot)}")
                new UtxoState(persistentProver, idToVersion(fb.id), store, constants, settings)
              }
              .recoverWith[UtxoState] { case e =>
                persistentProver.rollback(inRoot).ensuring(java.util.Arrays.equals(persistentProver.digest, inRoot))
                Failure(e)
              }
          }
        }
        .recoverWith { case e =>
          log.warn(s"Error while applying full block with header ${fb.header.encodedId} to UtxoState." +
            s"Reason: ${LoggingUtil.getReasonMsg(e)}")
          Failure(e)
        }
  }

  private def applyHeader: ModifierProcessing = {
    case h: Header =>
      Success(new UtxoState(persistentProver, idToVersion(h.id), this.store, constants, settings))
  }

  private def applySnapshot: ModifierProcessing = {
    case UtxoSnapshot(manifest, chunks, lastHeaders) =>
      log.info(s"Trying to recover state from snapshot with rootDigest ${Algos.encode(manifest.rootDigest)}")
      val serializer = new BatchAVLProverSerializer[Digest32, HF]
      serializer.combine(manifest.proverManifest -> chunks.map(_.subtree))
        .flatMap { prover =>
          ErgoStateContext.empty(constants).process(lastHeaders.last, None).flatMap { recoveredCtx =>
            implicit val vs: VotingSettings = settings.chainSettings.voting
            val manifestRootDigest = lastHeaders.head.stateRoot
            val newStore = recreateStore()
            val recoveredPersistentProver = {
              val storage: VersionedIODBAVLStorage[Digest32] =
                new VersionedIODBAVLStorage(newStore, UtxoState.nodeParameters)(Algos.hash)
              UtxoState.createPersistentProver(
                prover, storage, idToVersion(manifest.blockId), None, recoveredCtx)
            }
            if (!java.util.Arrays.equals(recoveredPersistentProver.digest, manifestRootDigest)) {
              Failure(new Exception(
                s"Unexpected prover digest after state recovery ${Algos.encode(recoveredPersistentProver.digest)}"))
            } else {
              log.info(s"State successfully recovered at height ${lastHeaders.head.height}")
              Success(new UtxoState(
                recoveredPersistentProver, idToVersion(manifest.blockId), newStore, constants, settings))
            }
          }
        }
  }

  private def other: ModifierProcessing = {
    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Exception("unknown modifier"))
  }

  private def recreateStore(): LSMStore = {
    val stateDir = ErgoState.stateDir(settings)
    stateDir.mkdirs()
    closeStorage()
    stateDir.listFiles().foreach(_.delete())
    new LSMStore(stateDir, keepVersions = constants.keepVersions)
  }

  override def rollbackVersions: Iterable[VersionTag] = {
    persistentProver.synchronized {
      persistentProver.storage.rollbackVersions
        .map { v =>
          store.get(ByteArrayWrapper(Algos.hash(v))).map(w => bytesToVersion(w.data))
        }
        .collect {
          case Some(value) => value
        }
    }
  }

}

object UtxoState {

  type ModifierProcessing = PartialFunction[ErgoPersistentModifier, Try[UtxoState]]

  private val nodeParameters = NodeParameters(keySize = Constants.HashLength, valueSize = None, labelSize = 32)

  private lazy val bestVersionKey = Algos.hash("best state version")
  val EmissionBoxIdKey = Algos.hash("emission box id key")

  private def metadata(modId: VersionTag,
                       stateRoot: ADDigest,
                       currentEmissionBoxOpt: Option[ErgoBox],
                       context: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = {
    val modIdBytes = versionToBytes(modId)
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modIdBytes -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modIdBytes
    val bestVersion = bestVersionKey -> modIdBytes
    val eb = EmissionBoxIdKey -> currentEmissionBoxOpt.map(_.id).getOrElse(Array[Byte]())
    val cb = ErgoStateReader.ContextKey -> context.bytes

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, eb, cb)
  }

  def create(dir: File, constants: StateConstants, settings: ErgoSettings): UtxoState = {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val version = store.get(ByteArrayWrapper(bestVersionKey)).map(w => bytesToVersion(w.data))
      .getOrElse(ErgoState.genesisStateVersion)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val bp = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, nodeParameters)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).get
    }
    new UtxoState(persistentProver, version, store, constants, settings)
  }

  def createPersistentProver(dir: File, constants: StateConstants): PersistentBatchAVLProver[Digest32, HF] = {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val bp = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, nodeParameters)(Algos.hash)
    PersistentBatchAVLProver.create(bp, storage)
      .fold(e => throw new Exception(s"Failed to create PersistentProver: $e"), p => p)
  }

  def createPersistentProver(prover: BatchAVLProver[Digest32, HF],
                             storage: VersionedIODBAVLStorage[Digest32],
                             stateVersion: VersionTag,
                             currentEmissionBoxOpt: Option[ErgoBox],
                             stateContext: ErgoStateContext): PersistentBatchAVLProver[Digest32, HF] = {
    PersistentBatchAVLProver.create(
      prover,
      storage,
      metadata(stateVersion, prover.digest, currentEmissionBoxOpt, stateContext),
      paranoidChecks = true
    ).fold(e => throw new Exception(s"Failed to create PersistentProver: $e"), p => p)
  }

  def fromBoxHolder(bh: BoxHolder,
                    currentEmissionBoxOpt: Option[ErgoBox],
                    dir: File,
                    constants: StateConstants,
                    settings: ErgoSettings): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = constants.keepVersions)

    implicit val votingSettings: VotingSettings = constants.votingSettings

    val defaultStateContext = new ErgoStateContext(Seq.empty, p.digest, LaunchParameters, VotingData.empty)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, nodeParameters)(Algos.hash)
    val persistentProver =
      createPersistentProver(p, storage, ErgoState.genesisStateVersion, currentEmissionBoxOpt, defaultStateContext)

    new UtxoState(persistentProver, ErgoState.genesisStateVersion, store, constants, settings)
  }

}
