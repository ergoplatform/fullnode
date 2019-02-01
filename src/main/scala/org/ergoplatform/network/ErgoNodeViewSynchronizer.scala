package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewHolder._
import scorex.core.PersistentNodeViewModifier
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.network.{ModifiersStatus, NodeViewSynchronizer}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ModifierId

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings,
                               timeProvider: NetworkTimeProvider)
                              (implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
    Constants.modifierSerializers) {

  override protected val deliveryTracker = new ErgoDeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks,
    self, timeProvider)

  /**
    * Approximate number of modifiers to be downloaded simultaneously
    */
  protected val desiredSizeOfExpectingQueue: Int = networkSettings.desiredInvObjects

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  /**
    * Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    * Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.
    */
  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(h)) == ModifiersStatus.Unknown

        h.nextModifiersToDownload(desiredSizeOfExpectingQueue - deliveryTracker.requestedSize, downloadRequired)
          .groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  override protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit = {
    super.requestMoreModifiers(applied)
    if (deliveryTracker.requestedSize < desiredSizeOfExpectingQueue / 2) {
      historyReaderOpt foreach { h =>
        if (h.isHeadersChainSynced) {
          // our requested list is is half empty - request more missed modifiers
          self ! CheckModifiersToDownload
        } else {
          // headers chain is not synced yet, but our requested list is half empty - ask for more headers
          sendSync(statusTracker, h)
        }
      }
    }
  }

  /**
    * If new enough semantically valid ErgoFullBlock was applied, send inv for block header and all its sections
    */
  private val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)
  }

  protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) => fb.toSeq.foreach(s => broadcastModifierInv(s))
      case _ =>
    }
  }

  override protected def viewHolderEvents: Receive =
    onSemanticallySuccessfulModifier orElse
      onCheckModifiersToDownload orElse
      super.viewHolderEvents
}

object ErgoNodeViewSynchronizer {

  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings,
      timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider), name)


  case object CheckModifiersToDownload

}
