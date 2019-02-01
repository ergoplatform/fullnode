package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.ErgoBoxSerializer
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.nodeView.state.ErgoState.ModifierProcessing
import org.ergoplatform.settings._
import org.ergoplatform.utils.LoggingUtil
import scorex.core._
import scorex.core.transaction.state.ModifierValidation
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO set authenticated as a dynamic dictionary.
  * See https://eprint.iacr.org/2016/994 for details on this mode.
  */
class DigestState protected(override val version: VersionTag,
                            override val rootHash: ADDigest,
                            override val store: Store,
                            ergoSettings: ErgoSettings,
                            val verifier: ErgoInterpreter)
  extends ErgoState[DigestState]
    with ModifierValidation[ErgoPersistentModifier]
    with ScorexLogging {

  override val constants: StateConstants = StateConstants(None, ergoSettings)

  private lazy val nodeSettings = ergoSettings.nodeSettings

  store.lastVersionID
    .foreach(id => assert(version == bytesToVersion(id.data), "version should always be equal to store.lastVersionID"))

  override lazy val maxRollbackDepth: Int = store.rollbackVersions().size

  def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      fb.adProofs match {
        case None =>
          Failure(new Exception("Empty proofs when trying to apply full block to Digest state"))
        case Some(proofs) if !java.util.Arrays.equals(ADProofs.proofDigest(proofs.proofBytes), fb.header.ADProofsRoot) =>
          Failure(new Exception("Incorrect proofs digest"))
        case Some(proofs) =>
          stateContext.appendFullBlock(fb, votingSettings).map { currentStateContext =>
            val txs = fb.blockTransactions.txs

            val declaredHash = fb.header.stateRoot
            // Check modifications, returning sequence of old values
            val oldValues: Seq[ErgoBox] = proofs.verify(ErgoState.stateChanges(txs), rootHash, declaredHash)
              .get.map(v => ErgoBoxSerializer.parseBytes(v).get)
            val knownBoxes = (txs.flatMap(_.outputs) ++ oldValues).map(o => (ByteArrayWrapper(o.id), o)).toMap
            val totalCost = txs.map { tx =>
              tx.statelessValidity.get
              val boxesToSpend = tx.inputs.map(_.boxId).map { id =>
                knownBoxes.get(ByteArrayWrapper(id)) match {
                  case Some(box) => box
                  case None => throw new Exception(s"Box with id ${Algos.encode(id)} not found")
                }
              }
              tx.statefulValidity(boxesToSpend, currentStateContext)(verifier).get
            }.sum
            if (totalCost > currentStateContext.currentParameters.maxBlockCost) {
              throw new Exception(s"Transaction cost $totalCost exceeds limit")
            }
          }
      }

    case _: Header => Success(Unit)

    case a: Any =>
      Failure(new Exception(s"Modifier not validated: $a"))
  }

  //todo: utxo snapshot application
  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] = processing(mod)

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    log.info(s"Rollback Digest State to version ${Algos.encoder.encode(version)}")
    val wrappedVersion = Algos.versionToBAW(version)
    Try(store.rollback(wrappedVersion)).map { _ =>
      store.clean(nodeSettings.keepVersions)
      val rootHash = ADDigest @@ store.get(wrappedVersion).get.data
      log.info(s"Rollback to version ${Algos.encoder.encode(version)} with roothash ${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, store, ergoSettings, verifier)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(w => bytesToVersion(w.data))

  def close(): Unit = store.close()

  private def processing: ModifierProcessing[DigestState] = processFullBlock orElse processHeader orElse processOther

  private def processFullBlock: ModifierProcessing[DigestState] = {
    case fb: ErgoFullBlock if nodeSettings.verifyTransactions =>
      log.info(s"Got new full block ${fb.encodedId} at height ${fb.header.height} with root " +
        s"${Algos.encode(fb.header.stateRoot)}. Our root is ${Algos.encode(rootHash)}")
      validate(fb)
        .flatMap { _ =>
          val version: VersionTag = idToVersion(fb.header.id)
          stateContext.appendFullBlock(fb, votingSettings).flatMap(update(version, fb.header.stateRoot, _))
        }
        .recoverWith { case e =>
          log.warn(s"Invalid block ${fb.encodedId}, reason: ${LoggingUtil.getReasonMsg(e)}")
          Failure(e)
        }
  }

  private def processHeader: ModifierProcessing[DigestState] = {
    case h: Header =>
      log.info(s"Got new Header ${h.encodedId} with root ${Algos.encoder.encode(h.stateRoot)}")
      val version: VersionTag = idToVersion(h.id)
      stateContext.appendBlock(h, None, votingSettings).flatMap(update(version, h.stateRoot, _))
  }

  private def processOther: ModifierProcessing[DigestState] = {
    case other =>
      log.warn(s"Unhandled modifier: $other")
      Success(this)
  }

  private def update(newVersion: VersionTag,
                     newRootHash: ADDigest,
                     newStateContext: ErgoStateContext): Try[DigestState] = Try {
    val wrappedVersion = Algos.versionToBAW(newVersion)
    val toUpdate = DigestState.metadata(newVersion, newRootHash, newStateContext)

    store.update(wrappedVersion, Seq.empty, toUpdate)
    new DigestState(newVersion, newRootHash, store, ergoSettings, verifier)
  }

}

object DigestState extends ScorexLogging with ScorexEncoding {

  /**
    * Creates [[DigestState]] from existing [[ErgoStateContext]] corresponding to some `version` and `rootHash`.
    */
  def recover(version: VersionTag,
              rootHash: ADDigest,
              stateContext: ErgoStateContext,
              dir: File,
              constants: StateConstants): DigestState = {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val verifier = ErgoInterpreter(LaunchParameters)
    val wrappedVersion = Algos.versionToBAW(version)
    val toUpdate = DigestState.metadata(version, rootHash, stateContext)

    store.update(wrappedVersion, Seq.empty, toUpdate)
    new DigestState(version, rootHash, store, constants.settings, verifier)
  }

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             constants: StateConstants): DigestState = Try {
    val store = new LSMStore(dir, keepVersions = constants.keepVersions)
    val context = ErgoStateReader.storageStateContext(store, constants)
    val verifier = ErgoInterpreter(LaunchParameters)
    (versionOpt, rootHashOpt) match {
      case (Some(version), Some(rootHash)) =>
        val state = if (store.lastVersionID.map(w => bytesToVersion(w.data)).contains(version)) {
          new DigestState(version, rootHash, store, constants.settings, verifier)
        } else {
          val inVersion = store.lastVersionID.map(w => bytesToVersion(w.data)).getOrElse(version)
          new DigestState(inVersion, rootHash, store, constants.settings, verifier)
            .update(version, rootHash, context).get //sync store
        }
        state.ensuring(bytesToVersion(store.lastVersionID.get.data) == version)
      case (None, None) if store.lastVersionID.isEmpty =>
        ErgoState.generateGenesisDigestState(dir, constants.settings)
      case (None, None) =>
        val version = bytesToVersion(store.lastVersionID.get.data)
        val rootHash = store.get(Algos.versionToBAW(version)).get.data
        new DigestState(version, ADDigest @@ rootHash, store, constants.settings, verifier)
      case _ => ???
    }
  } match {
    case Success(state) => state
    case Failure(e) =>
      log.warn(s"Failed to create state with ${versionOpt.map(encoder.encode)} and ${rootHashOpt.map(encoder.encode)}", e)
      ErgoState.generateGenesisDigestState(dir, constants.settings)
  }

  protected def metadata(newVersion: VersionTag,
                         newRootHash: ADDigest,
                         newStateContext: ErgoStateContext): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(
    Algos.versionToBAW(newVersion) -> ByteArrayWrapper(newRootHash),
    ByteArrayWrapper(ErgoStateReader.ContextKey) -> ByteArrayWrapper(newStateContext.bytes)
  )

}
