package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{ADProofs, Header}
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.state.ErgoState.ModifierProcessing
import org.ergoplatform.settings._
import org.ergoplatform.utils.LoggingUtil
import scorex.db.{ByteArrayWrapper, LDBVersionedStore}
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
                            override val store: LDBVersionedStore,
                            ergoSettings: ErgoSettings)
  extends ErgoState[DigestState]
    with ModifierValidation[ErgoPersistentModifier]
    with ScorexLogging
    with ScorexEncoding {

  override val constants: StateConstants = StateConstants(None, ergoSettings)

  private lazy val nodeSettings = ergoSettings.nodeSettings

  store.lastVersionID
    .foreach(id => assert(version == bytesToVersion(id), "version should always be equal to store.lastVersionID"))

  override lazy val maxRollbackDepth: Int = store.rollbackVersions().size

  private[state] def validateTransactions(transactions: Seq[ErgoTransaction],
                                          expectedHash: ADDigest,
                                          proofs: ADProofs,
                                          currentStateContext: ErgoStateContext): Try[Unit] = {
    // Check modifications, returning sequence of old values
    val boxesFromProofs: Seq[ErgoBox] = proofs.verify(ErgoState.stateChanges(transactions), rootHash, expectedHash)
      .get.map(v => ErgoBoxSerializer.parseBytes(v))
    val knownBoxes = (transactions.flatMap(_.outputs) ++ boxesFromProofs).map(o => (ByteArrayWrapper(o.id), o)).toMap

    def checkBoxExistence(id: ErgoBox.BoxId): Try[ErgoBox] = knownBoxes
      .get(ByteArrayWrapper(id))
      .fold[Try[ErgoBox]](Failure(new Exception(s"Box with id ${Algos.encode(id)} not found")))(Success(_))

    ErgoState.execTransactions(transactions, currentStateContext)(checkBoxExistence)
      .toTry
      .map(_ => ())
  }

  def validate(mod: ErgoPersistentModifier): Try[Unit] = mod match {
    case fb: ErgoFullBlock =>
      fb.adProofs match {
        case None =>
          Failure(new Exception("Empty proofs when trying to apply full block to Digest state"))
        case Some(proofs) if !java.util.Arrays.equals(ADProofs.proofDigest(proofs.proofBytes), fb.header.ADProofsRoot) =>
          Failure(new Exception("Incorrect proofs digest"))
        case Some(proofs) =>
          stateContext.appendFullBlock(fb, votingSettings).flatMap { currentStateContext =>
            val txs = fb.blockTransactions.txs
            val declaredHash = fb.header.stateRoot
            validateTransactions(txs, declaredHash, proofs, currentStateContext)
          }
      }

    case _: Header => Success(Unit)

    case a: Any =>
      Failure(new Exception(s"Modifier not validated: $a"))
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[DigestState] =
    (processFullBlock orElse processHeader orElse processOther) (mod)

  @SuppressWarnings(Array("OptionGet"))
  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    log.info(s"Rollback Digest State to version ${version}")
    val versionBytes = scorex.core.versionToBytes(version)
    Try(store.rollbackTo(versionBytes)).map { _ =>
      store.clean(nodeSettings.keepVersions)
      val rootHash = ADDigest @@ store.get(versionBytes).get
      log.info(s"Rollback to version ${version} with roothash ${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, store, ergoSettings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(w => bytesToVersion(w))

  def close(): Unit = store.close()

  private def processFullBlock: ModifierProcessing[DigestState] = {
    case fb: ErgoFullBlock if nodeSettings.verifyTransactions =>
      log.info(s"Got new full block ${fb.encodedId} at height ${fb.header.height} with root " +
        s"${Algos.encode(fb.header.stateRoot)}. Our root is ${Algos.encode(rootHash)}")
      validate(fb)
        .flatMap { _ =>
          val version: VersionTag = idToVersion(fb.header.id)
          stateContext.appendFullBlock(fb, votingSettings) match {
            case Success(sc) => update(version, fb.header.stateRoot, sc)
            case Failure(e) =>
              log.error(s"Can't modify state context due to ${e.getMessage} ", e)
              Failure(e)
          }
        }.recoverWith { case e =>
          log.warn(s"Invalid block ${fb.encodedId}, reason: ${LoggingUtil.getReasonMsg(e)}")
          Failure(e)
        }
  }

  private def processHeader: ModifierProcessing[DigestState] = {
    case h: Header =>
      log.info(s"Got new Header ${h.encodedId} with root ${Algos.encoder.encode(h.stateRoot)}")
      val version: VersionTag = idToVersion(h.id)
      stateContext.appendHeader(h, votingSettings) match {
        case Success(sc) => update(version, h.stateRoot, sc)
        case Failure(e) =>
          log.error(s"Can't modify state context due to ${e.getMessage} ", e)
          Failure(e)
      }
   }

  private def processOther: ModifierProcessing[DigestState] = {
    case other =>
      log.warn(s"Unhandled modifier: $other")
      Success(this)
  }

  private def update(newVersion: VersionTag,
                     newRootHash: ADDigest,
                     newStateContext: ErgoStateContext): Try[DigestState] = Try {

    val toUpdate = DigestState.metadata(newVersion, newRootHash, newStateContext)

    store.update(scorex.core.versionToBytes(newVersion), Seq.empty, toUpdate)
    new DigestState(newVersion, newRootHash, store, ergoSettings)
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
    val store = new LDBVersionedStore(dir, keepVersions = constants.keepVersions)
    val toUpdate = DigestState.metadata(version, rootHash, stateContext)

    store.update(scorex.core.versionToBytes(version), Seq.empty, toUpdate)
    new DigestState(version, rootHash, store, constants.settings)
  }

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             constants: StateConstants): DigestState = {
    val store = new LDBVersionedStore(dir, keepVersions = constants.keepVersions)
    Try {
      val context = ErgoStateReader.storageStateContext(store, constants)
      (versionOpt, rootHashOpt) match {
        case (Some(version), Some(rootHash)) =>
          val state = if (store.lastVersionID.map(w => bytesToVersion(w)).contains(version)) {
            new DigestState(version, rootHash, store, constants.settings)
          } else {
            val inVersion = store.lastVersionID.map(w => bytesToVersion(w)).getOrElse(version)
            new DigestState(inVersion, rootHash, store, constants.settings)
              .update(version, rootHash, context).get //sync store
          }
          state.ensuring(bytesToVersion(store.lastVersionID.get) == version)
        case (None, None) if store.lastVersionID.isEmpty =>
          ErgoState.generateGenesisDigestState(dir, constants.settings)
        case _ =>
          val version = store.lastVersionID.get
          val rootHash = store.get(version).get
          new DigestState(bytesToVersion(version), ADDigest @@ rootHash, store, constants.settings)
      }
    } match {
      case Success(state) => state
      case Failure(e) =>
        store.close()
        log.warn(s"Failed to create state with ${versionOpt.getOrElse('?')} and ${rootHashOpt.map(encoder.encode)}", e)
        ErgoState.generateGenesisDigestState(dir, constants.settings)
    }
  }

  protected def metadata(newVersion: VersionTag,
                         newRootHash: ADDigest,
                         newStateContext: ErgoStateContext): Seq[(Array[Byte], Array[Byte])] = Seq(
    scorex.core.versionToBytes(newVersion) -> newRootHash,
    ErgoStateReader.ContextKey -> newStateContext.bytes
  )

}
