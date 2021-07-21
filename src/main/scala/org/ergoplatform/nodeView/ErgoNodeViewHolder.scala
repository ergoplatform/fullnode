package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoApp
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import org.ergoplatform.nodeView.state._
import org.ergoplatform.utils.metrics.CsvFileCollector
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, Algos, ErgoSettings}
import org.ergoplatform.utils.{metrics, FileUtils}
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SuccessfulTransaction}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider

import scala.util.{Failure, Success, Try}

abstract class ErgoNodeViewHolder[State <: ErgoState[State]](settings: ErgoSettings,
                                                             timeProvider: NetworkTimeProvider)
  extends NodeViewHolder[ErgoTransaction, ErgoPersistentModifier] {

  private implicit lazy val actorSystem: ActorSystem = context.system

  override val scorexSettings: ScorexSettings = settings.scorexSettings

  private lazy val metricsCollector = new CsvFileCollector(settings.directory + "/metrics")

  override type MS = State
  override type SI = ErgoSyncInfo
  override type HIS = ErgoHistory
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  override protected lazy val modifiersCache =
    new ErgoModifiersCache(settings.scorexSettings.network.maxModifiersCacheSize)

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

  override protected def pmodModify(pmod: ErgoPersistentModifier): Unit = {
    if (settings.nodeSettings.collectMetrics)
      metrics.executeWithCollector(metricsCollector) {
        super.pmodModify(pmod)
      }
    else
      super.pmodModify(pmod)
  }

  override protected def txModify(tx: ErgoTransaction): Unit = {
    memoryPool().process(tx, minimalState()) match {
      case (newPool, ProcessingOutcome.Accepted) =>
        log.debug(s"Unconfirmed transaction $tx added to the memory pool")
        val newVault = vault().scanOffchain(tx)
        updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
        context.system.eventStream.publish(SuccessfulTransaction[ErgoTransaction](tx))
      case (newPool, ProcessingOutcome.Invalidated(e)) =>
        log.debug(s"Transaction $tx invalidated. Cause: ${e.getMessage}")
        updateNodeView(updatedMempool = Some(newPool))
        context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
      case (_, ProcessingOutcome.DoubleSpendingLoser(winnerTxs)) => // do nothing
        log.debug(s"Transaction $tx declined, as other transactions $winnerTxs are paying more")
      case (_, ProcessingOutcome.Declined(e)) => // do nothing
        log.debug(s"Transaction $tx declined, reason: ${e.getMessage}")
    }
  }

  /**
    * Performs mempool update after a block application, transactions
    * from rolled back block are to be returned to the pool, and transactions
    * included in applied block are to be removed.
    */
  override protected def updateMemPool(blocksRemoved: Seq[ErgoPersistentModifier],
                                       blocksApplied: Seq[ErgoPersistentModifier],
                                       memPool: MP,
                                       state: MS): MP = {
    val rolledBackTxs = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs = blocksApplied.flatMap(extractTransactions)

    memPool.putWithoutCheck(rolledBackTxs).filter(tx => !appliedTxs.exists(_.id == tx.id))
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
    log.info("History database read")
    val memPool = ErgoMemPool.empty(settings)
    val constants = StateConstants(Some(self), settings)
    val state = restoreConsistentState(ErgoState.readOrGenerate(settings, constants).asInstanceOf[MS], history)
    log.info("State database read, state synchronized")
    val wallet = ErgoWallet.readOrGenerate(
      history.getReader.asInstanceOf[ErgoHistoryReader],
      settings)
    log.info("Wallet database read")
    Some((history, state, wallet, memPool))
  }

  @SuppressWarnings(Array("AsInstanceOf"))
  private def recreatedState(): State = {
    val dir = stateDir(settings)
    FileUtils.deleteRecursive(dir)

    val constants = StateConstants(Some(self), settings)
    ErgoState.readOrGenerate(settings, constants)
      .asInstanceOf[State]
      .ensuring(
        state => java.util.Arrays.equals(state.rootHash, settings.chainSettings.genesisStateDigest),
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
    * Recovers digest state from history.
    */
  private def recoverDigestState(bestFullBlock: ErgoFullBlock, history: ErgoHistory): Try[DigestState] = {
    val constants = StateConstants(Some(self), settings)
    val votingLength = settings.chainSettings.voting.votingLength
    val bestHeight = bestFullBlock.header.height
    val newEpochHeadersQty = bestHeight % votingLength // how many blocks current epoch lasts
    val headersQtyToAcquire = newEpochHeadersQty + Constants.LastHeadersInContext
    val acquiredChain = history.headerChainBack(headersQtyToAcquire, bestFullBlock.header, _ => false).headers
    val (lastHeaders, chainToApply) = acquiredChain.splitAt(Constants.LastHeadersInContext)
    val firstExtensionOpt = lastHeaders.lastOption // last extension in the prev epoch to recover from
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))

    val recoveredStateTry = firstExtensionOpt
      .fold[Try[ErgoStateContext]](Failure(new Exception("Could not find extension to recover from"))
      )(ext => ErgoStateContext.recover(constants.genesisStateDigest, ext, lastHeaders)(settings))
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


/** This class guarantees to its inheritors the creation of correct instance of `ErgoNodeViewHolder`
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
