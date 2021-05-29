package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.{ModifiersFromRemote, TransactionsFromRemote}
import scorex.core.NodeViewHolder._
import scorex.core.consensus.History.{HistoryComparisonResult, Older}
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.{ModifierTypeId, NodeViewModifier, PersistentNodeViewModifier}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.core.network.{ConnectedPeer, ModifiersStatus, NodeViewSynchronizer, SendToPeer, SendToRandom}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import scorex.core.validation.MalformedModifierError
import scorex.util.ModifierId

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Tweaks on top of Scorex' NodeViewSynchronizer made to optimize Ergo network
  */
class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               settings: ErgoSettings,
                               timeProvider: NetworkTimeProvider)
                              (implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec,
    settings.scorexSettings.network, timeProvider, Constants.modifierSerializers) {

  override protected val deliveryTracker =
    new ErgoDeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  protected val networkSettings: NetworkSettings = settings.scorexSettings.network

  /**
    * Approximate number of modifiers to be downloaded simultaneously
    */
  protected val desiredSizeOfExpectingQueue: Int = networkSettings.desiredInvObjects

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.scheduleAtFixedRate(toDownloadCheckInterval, toDownloadCheckInterval, self, CheckModifiersToDownload)
  }

  /**
    * Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    * Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.
    */
  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(h)) == ModifiersStatus.Unknown

        val toDownload =
          h.nextModifiersToDownload(desiredSizeOfExpectingQueue - deliveryTracker.requestedSize, downloadRequired)

        log.info(s"${toDownload.length} persistent modifiers to be downloaded")

        toDownload.groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  // todo: this method is just a copy of the ancestor from Scorex, however, smarter logic is needed, not just
  //  asking from a random peer
  override protected def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, None)
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  /**
    * Logic to process block parts got from another peer.
    * Filter out non-requested block parts (with a penalty to spamming peer),
    * parse block parts and send valid modifiers to NodeViewHolder
    *
    * Currently just a copy from private method in basic trait!
    */
  override protected def modifiersFromRemote(data: ModifiersData, remote: ConnectedPeer): Unit = {
    val typeId = data.typeId
    val modifiers = data.modifiers
    log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: $remote")
    log.trace(s"Received modifier ids ${modifiers.keySet.map(encoder.encodeId).mkString(",")}")

    // filter out non-requested modifiers
    val requestedModifiers = processSpam(remote, typeId, modifiers)

    Constants.modifierSerializers.get(typeId) match {
      case Some(serializer: ScorexSerializer[ErgoTransaction]@unchecked) if typeId == Transaction.ModifierTypeId =>
        // parse all transactions and send them to node view holder
        val parsed: Iterable[ErgoTransaction] = parseModifiers(requestedModifiers, serializer, remote)
        viewHolderRef ! TransactionsFromRemote(parsed)

      case Some(serializer: ScorexSerializer[ErgoPersistentModifier]@unchecked) =>
        // parse all modifiers and put them to modifiers cache
        val parsed: Iterable[ErgoPersistentModifier] = parseModifiers(requestedModifiers, serializer, remote)
        val valid = parsed.filter(validateAndSetStatus(remote, _))
        if (valid.nonEmpty) {
          viewHolderRef ! ModifiersFromRemote[ErgoPersistentModifier](valid)
          if(valid.head.modifierTypeId == Header.modifierTypeId){
            val h = valid.head.asInstanceOf[Header].height
            historyReaderOpt.foreach { hr =>
              if(hr.fullBlockHeight < h){
                statusTracker.updateStatus(remote, Older)
                sendSync(statusTracker, hr)
              }
            }
          }
        }
      case _ =>
        log.error(s"Undefined serializer for modifier of type $typeId")
    }
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  override def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    ext: Seq[(ModifierTypeId, ModifierId)]): Unit = {
    ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, modsUnchecked) =>
        val mods = if(modsUnchecked.length > 400) {
          log.error("Extension has length more than 400")
          modsUnchecked.take(400)
        } else {
          modsUnchecked
        }
        networkControllerRef ! SendToNetwork(Message(invSpec, Right(InvData(mid, mods)), None), SendToPeer(remote))
    }
  }

  /**
    * Currently just a copy from private method in basic trait! Will be optimized in future likely.
    *
    * Parse modifiers using specified serializer, check that its id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier,
    * call deliveryTracker.onReceive() for every correct modifier to update its status
    *
    * @return collection of parsed modifiers
    */
  def parseModifiers[M <: NodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                            serializer: ScorexSerializer[M],
                                            remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      serializer.parseBytesTry(bytes) match {
        case Success(mod) if id == mod.id =>
          Some(mod)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse modifier with declared id ${encoder.encodeId(id)} from ${remote.toString}")
          None
      }
    }
  }

  /**
    * Currently just copy from private method in basic trait! Will be optimized in future likely.
    *
    * Get modifiers from remote peer, filter out spam modifiers and penalize peer for spam
    *
    * @return ids and bytes of modifiers that were requested by our node
    */
  private def processSpam(remote: ConnectedPeer,
                          typeId: ModifierTypeId,
                          modifiers: Map[ModifierId, Array[Byte]]): Map[ModifierId, Array[Byte]] = {
    val (requested, spam) = modifiers.partition { case (id, _) =>
      deliveryTracker.status(id) == Requested
    }

    // todo: consider rules for penalizing peers for spammy transactions
    if (spam.nonEmpty) {
      if(typeId == Transaction.ModifierTypeId) {
        log.info(s"Got spammy transactions: $modifiers")
      } else {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(encoder.encodeId)}")
        penalizeSpammingPeer(remote)
      }
    }
    requested
  }

  /**
    * Currently just copy from private method in basic trait! Will be optimized in future likely.
    *
    * Move `pmod` to `Invalid` if it is permanently invalid, to `Received` otherwise
    */
  private def validateAndSetStatus(remote: ConnectedPeer, pmod: ErgoPersistentModifier): Boolean = {
    historyReaderOpt match {
      case Some(hr) =>
        hr.applicableTry(pmod) match {
          case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
            log.warn(s"Modifier ${pmod.encodedId} is permanently invalid", e)
            deliveryTracker.setInvalid(pmod.id)
            penalizeMisbehavingPeer(remote)
            false
          case _ =>
            deliveryTracker.setReceived(pmod.id, remote)
            true
        }
      case None =>
        log.error("Got modifier while history reader is not ready")
        deliveryTracker.setReceived(pmod.id, remote)
        true
    }
  }

  /**
    * Object ids coming from other node.
    * Filter out modifier ids that are already in process (requested, received or applied),
    * request unknown ids from peer and set this ids to requested state.
    */
  override protected def processInv(invData: InvData, peer: ConnectedPeer): Unit = {
    (mempoolReaderOpt, historyReaderOpt) match {
      case (Some(mempool), Some(history)) =>

        val modifierTypeId = invData.typeId

        val newModifierIds = modifierTypeId match {
          case Transaction.ModifierTypeId =>
            // We download transactions only if the node is not needed for externally provided proofs
            // (so having UTXO set, and the chain is synced
            if (!settings.nodeSettings.stateType.requireProofs &&
              history.isHeadersChainSynced &&
              history.fullBlockHeight == history.headersHeight) {
              invData.ids.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            } else {
              Seq.empty
            }
          case _ =>
            invData.ids.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
        }

        if (newModifierIds.nonEmpty) {
          val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, newModifierIds)), None)
          peer.handlerRef ! msg
          deliveryTracker.setRequested(newModifierIds, modifierTypeId, Some(peer))
        }

      case _ =>
        log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
    }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  override protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit = {
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
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, settings,
      timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, settings, timeProvider))

  case object CheckModifiersToDownload

}
