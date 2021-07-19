package org.ergoplatform.local

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.GetNodeViewChanges
import scorex.core.network.Broadcast
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, SemanticallySuccessfulModifier}
import scorex.core.network.message.{InvData, InvSpec, Message}
import scorex.core.transaction.Transaction
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ScorexLogging

import scala.concurrent.duration._

/**
  * Controls mempool cleanup workflow. Watches NodeView events and delegates
  * mempool cleanup task to [[CleanupWorker]] when needed.
  */
class MempoolAuditor(nodeViewHolderRef: ActorRef,
                     networkControllerRef: ActorRef,
                     settings: ErgoSettings) extends Actor with ScorexLogging {

  override def postRestart(reason: Throwable): Unit = {
    log.error(s"Mempool auditor actor restarted due to ${reason.getMessage}", reason)
    super.postRestart(reason)
  }

  override def postStop(): Unit = {
    logger.info("Mempool auditor stopped")
    super.postStop()
  }

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 1.minute) {
    case _: ActorKilledException => Stop
    case _: DeathPactException => Stop
    case e: ActorInitializationException =>
      log.warn(s"Cleanup worker failed during initialization with: $e")
      Stop
    case e: Exception =>
      log.warn(s"Cleanup worker failed with: $e")
      context become awaiting // turn ctx into awaiting mode if worker failed
      Restart
  }

  private var stateReaderOpt: Option[TransactionValidation[ErgoTransaction]] = None
  private var poolReaderOpt: Option[ErgoMemPoolReader] = None

  private val worker: ActorRef =
    context.actorOf(Props(new CleanupWorker(nodeViewHolderRef, settings.nodeSettings)))

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = awaiting

  private def awaiting: Receive = {
    case SemanticallySuccessfulModifier(_: ErgoFullBlock) | SemanticallySuccessfulModifier(_: Header) =>
      stateReaderOpt = None
      poolReaderOpt = None
      nodeViewHolderRef ! GetNodeViewChanges(history = false, state = true, mempool = true, vault = false)

    case ChangedMempool(mp: ErgoMemPoolReader) =>
      poolReaderOpt = Some(mp)
      stateReaderOpt.foreach(st => initiateCleanup(st, mp))

    case ChangedState(st: TransactionValidation[ErgoTransaction@unchecked]) =>
      stateReaderOpt = Some(st)
      poolReaderOpt.foreach(mp => initiateCleanup(st, mp))

    case ChangedState(_) | ChangedMempool(_) => // do nothing
  }

  private def working: Receive = {
    case CleanupDone =>
      log.info("Cleanup done. Switching to awaiting mode")
      //rebroadcast transactions
      rebroadcastTransactions()
      stateReaderOpt = None
      poolReaderOpt = None
      context become awaiting

    case _ => // ignore other triggers until work is done
  }

  private def initiateCleanup(validator: TransactionValidation[ErgoTransaction],
                              mempool: ErgoMemPoolReader): Unit = {
    log.info("Initiating cleanup. Switching to working mode")
    worker ! RunCleanup(validator, mempool)
    context become working // ignore other triggers until work is done
  }

  private def rebroadcastTransactions(): Unit = {
    log.debug("Rebroadcasting transactions")
    poolReaderOpt.foreach { pr =>
      pr.take(settings.nodeSettings.rebroadcastCount).foreach { tx =>
        log.info(s"Rebroadcasting $tx")
        val msg = Message(
          new InvSpec(settings.scorexSettings.network.maxInvObjects),
          Right(InvData(Transaction.ModifierTypeId, Seq(tx.id))),
          None
        )
        networkControllerRef ! SendToNetwork(msg, Broadcast)
      }

    }

  }
}

object MempoolAuditor {

  case object CleanupDone

}

object MempoolAuditorRef {

  def props(nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: ErgoSettings): Props =
    Props(new MempoolAuditor(nodeViewHolderRef, networkControllerRef, settings))

  def apply(nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: ErgoSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(nodeViewHolderRef, networkControllerRef, settings))

}
