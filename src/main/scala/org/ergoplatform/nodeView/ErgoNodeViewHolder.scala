package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoApp
import org.ergoplatform.local.SnapshotCreator
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SemanticallyFailedModification, SemanticallySuccessfulModifier, SuccessfulTransaction}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest

import scala.util.{Failure, Success, Try}

abstract class ErgoNodeViewHolder[State <: ErgoState[State]](settings: ErgoSettings,
                                                             timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[ErgoTransaction, ErgoPersistentModifier] {

  private implicit lazy val actorSystem: ActorSystem = context.system

  override val scorexSettings: ScorexSettings = settings.scorexSettings

  override type MS = State
  override type SI = ErgoSyncInfo
  override type HIS = ErgoHistory
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  override protected lazy val modifiersCache =
    new ErgoModifiersCache(settings.scorexSettings.network.maxModifiersCacheSize)

  private val snapshotCreatorRefOpt: Option[ActorRef] = {
    if (!settings.nodeSettings.stateType.requireProofs) {
      Some(actorSystem.actorOf(Props(classOf[SnapshotCreator])))
    } else {
      None
    }
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  override def postStop(): Unit = {
    log.warn("Stopping ErgoNodeViewHolder")
    history().closeStorage()
    minimalState().closeStorage()
  }

  override protected def txModify(tx: ErgoTransaction): Unit = {
    memoryPool().putIfValid(tx, minimalState()) match {
      case (newPool, ProcessingOutcome.Accepted) =>
        log.debug(s"Unconfirmed transaction $tx added to the memory pool")
        val newVault = vault().scanOffchain(tx)
        updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
        context.system.eventStream.publish(SuccessfulTransaction[ErgoTransaction](tx))
      case (newPool, ProcessingOutcome.Invalidated(e)) =>
        log.debug(s"Transaction $tx invalidated")
        updateNodeView(updatedMempool = Some(newPool))
        context.system.eventStream.publish(FailedTransaction[ErgoTransaction](tx, e))
      case (_, ProcessingOutcome.Declined) => // do nothing
        log.debug(s"Transaction $tx declined")
    }
  }

  /**
    * Tries to apply modifier to the state. Checks if snapshot creation needed, triggers it
    * and publish `SemanticallySuccessfulModifier` in case of successful application.
    * Marks modifier as invalid and publishes `SemanticallyFailedModification` otherwise.
    */
  override protected def applyState(history: ErgoHistory,
                                    stateToApply: State,
                                    suffixTrimmed: IndexedSeq[ErgoPersistentModifier],
                                    progressInfo: ProgressInfo[ErgoPersistentModifier]): UpdateInformation = {
    val updateInfoSample = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
    progressInfo.toApply.foldLeft(updateInfoSample) { case (updateInfo, modToApply) =>
      updateInfo.failedMod.fold {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHistory = history.reportModifierIsValid(modToApply)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
            modToApply match {
              case block: ErgoFullBlock if block.header.height % settings.nodeSettings.snapshotCreationInterval == 0 &&
                settings.nodeSettings.keepLastSnapshots != 0 =>
                createStateSnapshot(block.header, stateAfterApply)
              case _ => // do nothing.
            }
            UpdateInformation(newHistory, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)
          case Failure(e) =>
            val (newHistory, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHistory, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      }(_ => updateInfo)
    }
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (ErgoHistory, MS, ErgoWallet, ErgoMemPool) = {

    val state = recreatedState()

    val history = ErgoHistory.readOrGenerate(settings, timeProvider)

    val wallet = ErgoWallet.readOrGenerate(
      history.getReader.asInstanceOf[ErgoHistoryReader],
      settings)

    val memPool = ErgoMemPool.empty(settings)

    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  override def restoreState: Option[NodeView] = if (ErgoHistory.historyDir(settings).listFiles().isEmpty) {
    None
  } else {
    val history = ErgoHistory.readOrGenerate(settings, timeProvider)
    val memPool = ErgoMemPool.empty(settings)
    val constants = StateConstants(Some(self), settings)
    val state = restoreConsistentState(ErgoState.readOrGenerate(settings, constants).asInstanceOf[MS], history)
    val wallet = ErgoWallet.readOrGenerate(
      history.getReader.asInstanceOf[ErgoHistoryReader],
      settings)
    Some((history, state, wallet, memPool))
  }

  /**
    * Create copy of BatchAvlProver and send it to SnapshotCreator actor in order
    * to make snapshot taking process asynchronous if node operates in utxo mode.
    */
  private def createStateSnapshot(lastHeader: Header, state: State): Unit = {
    state match {
      case utxoReader: UtxoStateReader =>
        val dir = ErgoState.stateDir(settings)
        dir.mkdirs()
        val prover = UtxoState.createPersistentProver(dir, utxoReader.constants).avlProver
        snapshotCreatorRefOpt.foreach(_ ! SnapshotCreator.TakeSnapshotOf(prover, lastHeader))
      case _ =>
        log.warn("Attempting to create state snapshot in unsupported state mode")
    }
  }

  @SuppressWarnings(Array("AsInstanceOf"))
  private def recreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): State = {
    val dir = stateDir(settings)
    for (file <- dir.listFiles) file.delete

    {
      val constants = StateConstants(Some(self), settings)
      (version, digest, settings.nodeSettings.stateType) match {
        case (Some(_), Some(_), StateType.Digest) =>
          DigestState.create(version, digest, dir, constants)
        case _ =>
          ErgoState.readOrGenerate(settings, constants)
      }
    }
      .asInstanceOf[State]
      .ensuring(
        t => java.util.Arrays.equals(t.rootHash, digest.getOrElse(settings.chainSettings.monetary.afterGenesisStateDigest)),
        "State root is incorrect"
      )
  }

  private def restoreConsistentState(stateIn: State, history: ErgoHistory): State = {
    (stateIn.version, history.bestFullBlockOpt, stateIn) match {
      case (ErgoState.genesisStateVersion, None, _) =>
        log.info("State and history are both empty on startup")
        Success(stateIn)
      case (stateId, Some(block), _) if stateId == block.id =>
        log.info(s"State and history have the same version ${encoder.encode(stateId)}, no recovery needed.")
        Success(stateIn)
      case (_, None, _) =>
        log.info("State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        Success(recreatedState())
      case (_, Some(bestFullBlock), _: DigestState) =>
        log.info(s"State and history are inconsistent. Going to switch state to version ${bestFullBlock.encodedId}")
        recoverDigestState(bestFullBlock, history).map(_.asInstanceOf[State])
      case (stateId, Some(historyBestBlock), state) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](versionToId(stateId))
        val (rollbackId, newChain) = history.chainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        log.info(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val initState = rollbackId
          .map(id => state.rollbackTo(idToVersion(id)).get)
          .getOrElse(recreatedState())
        val toApply = newChain.headers.map { h =>
          history.getFullBlock(h)
            .fold(throw new Error(s"Failed to get full block for header $h"))(fb => fb)
        }
        toApply.foldLeft[Try[State]](Success(initState))((acc, m) => acc.flatMap(_.applyModifier(m)))
    }
  } match {
    case Failure(e) =>
      log.error("Failed to recover state, try to resync from genesis manually", e)
      ErgoApp.forceStopApplication(500)
    case Success(state) =>
      state
  }

  /**
    * Recovers digest state from history
    */
  private def recoverDigestState(bestFullBlock: ErgoFullBlock, history: ErgoHistory): Try[DigestState] = {
    val constants = StateConstants(Some(self), settings)
    val votingLength = settings.chainSettings.voting.votingLength
    val bestHeight = bestFullBlock.header.height
    val newEpochHeadersQty = bestHeight % votingLength
    val headersQtyToAcquire = newEpochHeadersQty + Constants.LastHeadersInContext
    val acquiredChain = history.headerChainBack(headersQtyToAcquire, bestFullBlock.header, _ => false).headers
    val (lastHeaders, chainToApply) = acquiredChain.splitAt(Constants.LastHeadersInContext)
    val firstExtensionOpt = lastHeaders.lastOption
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))

    val recoveredStateTry = firstExtensionOpt
      .fold[Try[ErgoStateContext]](Failure(new Exception("Could not find extension to recover from"))
      )(ext => ErgoStateContext.recover(constants.genesisStateDigest, ext, lastHeaders)(settings.chainSettings.voting))
      .map { ctx =>
        val recoverVersion = idToVersion(lastHeaders.last.id)
        val recoverRoot = bestFullBlock.header.stateRoot
        DigestState.recover(recoverVersion, recoverRoot, ctx, stateDir(settings), constants)
      }

    recoveredStateTry match {
      case Success(state) =>
        log.info("Recovering state using current epoch")
        chainToApply.foldLeft[Try[DigestState]](Success(state))((acc, m) => acc.flatMap(_.applyModifier(m)))
      case Failure(exception) => // recover using whole headers chain
        log.warn(s"Failed to recover state from current epoch, using whole chain: ${exception.getMessage}")
        val wholeChain = history.headerChainBack(Int.MaxValue, bestFullBlock.header, _.isGenesis).headers
        val genesisState = DigestState.create(None, None, stateDir(settings), constants)
        wholeChain.foldLeft[Try[DigestState]](Success(genesisState))((acc, m) => acc.flatMap(_.applyModifier(m)))
    }
  }

  private def stateDir(settings: ErgoSettings): File = {
    val dir = ErgoState.stateDir(settings)
    dir.mkdirs()
    dir
  }

  // scalastyle:on

}

private[nodeView] class DigestNodeViewHolder(settings: ErgoSettings,
                                             timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[DigestState](settings, timeProvider)

private[nodeView] class UtxoNodeViewHolder(settings: ErgoSettings,
                                           timeProvider: NetworkTimeProvider)
  extends ErgoNodeViewHolder[UtxoState](settings, timeProvider)


/**
  * This class guarantees to its inheritors the creation of correct instance of `ErgoNodeViewHolder`
  * for the given instance of `StateType`
  */
sealed abstract class ErgoNodeViewProps[ST <: StateType, S <: ErgoState[S], N <: ErgoNodeViewHolder[S]]
(implicit ev: StateType.Evidence[ST, S]) {
  def apply(settings: ErgoSettings, timeProvider: NetworkTimeProvider, digestType: ST): Props
}

object DigestNodeViewProps extends ErgoNodeViewProps[StateType.DigestType, DigestState, DigestNodeViewHolder] {
  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            digestType: StateType.DigestType): Props =
    Props.create(classOf[DigestNodeViewHolder], settings, timeProvider)
}

object UtxoNodeViewProps extends ErgoNodeViewProps[StateType.UtxoType, UtxoState, UtxoNodeViewHolder] {
  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider,
            digestType: StateType.UtxoType): Props =
    Props.create(classOf[UtxoNodeViewHolder], settings, timeProvider)
}

object ErgoNodeViewRef {

  def props(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider): Props =
    settings.nodeSettings.stateType match {
      case digestType@StateType.Digest => DigestNodeViewProps(settings, timeProvider, digestType)
      case utxoType@StateType.Utxo => UtxoNodeViewProps(settings, timeProvider, utxoType)
    }

  def apply(settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider))
}
