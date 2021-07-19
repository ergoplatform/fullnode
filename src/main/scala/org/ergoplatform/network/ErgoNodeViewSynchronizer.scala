package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.{CheckModifiersToDownload, PeerSyncState}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder.ReceivableMessages.{ModifiersFromRemote, TransactionsFromRemote}
import scorex.core.NodeViewHolder._
import scorex.core.consensus.History._
import scorex.core.network.ModifiersStatus.Requested
import scorex.core.{ModifierTypeId, NodeViewModifier, PersistentNodeViewModifier, idsToString}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.network.message.{InvData, Message, ModifiersData}
import scorex.core.network.{ConnectedPeer, ModifiersStatus, NodeViewSynchronizer, SendToPeer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import scorex.core.validation.MalformedModifierError
import scorex.util.ModifierId

import scala.collection.mutable
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

  private val networkSettings: NetworkSettings = settings.scorexSettings.network

  /**
    * Approximate number of modifiers to be downloaded simultaneously, headers are much faster to process
    */
  private val desiredSizeOfExpectingModifierQueue: Int = networkSettings.desiredInvObjects
  private val desiredSizeOfExpectingHeaderQueue: Int = desiredSizeOfExpectingModifierQueue * 5

  private val minModifiersPerBucket = 5 // minimum of persistent modifiers (excl. headers) to download by single peer
  private val maxModifiersPerBucket = 20 // maximum of persistent modifiers (excl. headers) to download by single peer

  private val minHeadersPerBucket = 50 // minimum of headers to download by single peer
  private val maxHeadersPerBucket = 400 // maximum of headers to download by single peer

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.scheduleAtFixedRate(toDownloadCheckInterval, toDownloadCheckInterval, self, CheckModifiersToDownload)
  }

  private def downloadRequired(historyReader: ErgoHistory)(id: ModifierId): Boolean = {
    deliveryTracker.status(id, Array(historyReader)) == ModifiersStatus.Unknown
  }

  /**
    * Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    * Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.
    */
  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val maxModifiers = desiredSizeOfExpectingModifierQueue - deliveryTracker.requestedSize
        log.debug(s"Going to download $maxModifiers non-header modifiers")
        requestDownload(
          maxModifiers,
          minModifiersPerBucket,
          maxModifiersPerBucket
        )(getPeersForDownloadingBlocks) { howManyPerType =>
          h.nextModifiersToDownload(howManyPerType, downloadRequired(h))
        }
      }
  }

  //Processing sync info is coming from another node
  override protected def processSync(syncInfo: ErgoSyncInfo, remote: ConnectedPeer): Unit = {

    historyReaderOpt match {
      case Some(historyReader) =>

        val comparison = historyReader.compare(syncInfo)
        log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
          s"Comparison result is $comparison.")

        val status = comparison
        statusTracker.updateStatus(remote, status)

        status match {
          case Unknown =>
            // we do not know what to send to a peer with unknown status
            log.info(s"Peer status is still unknown for $remote")
          case Nonsense =>
            // we do not know what to send to a peer with such status
            log.info(s"Got nonsense status for $remote")
          case Younger | Fork =>
            // send extension (up to 400 header ids) to a peer which chain is less developed or forked
            val ext = historyReader.continuationIds(syncInfo, size = 400)
            if (ext.isEmpty) log.warn("Extension is empty while comparison is younger")
            log.info(s"Sending extension of length ${ext.length}")
            log.debug(s"Extension ids: ${idsToString(ext)}")
            sendExtension(remote, status, ext)
          case Older =>
            // asking headers from older peers
            val ids = syncInfo.lastHeaderIds.reverse
            val headerIds = ids.takeWhile(hId => !historyReader.isInBestChain(hId))
            if (headerIds.nonEmpty) {
              val maxModifiers = desiredSizeOfExpectingHeaderQueue - deliveryTracker.requestedSize
              log.debug(s"Requesting $maxModifiers headers from older peers after getting sync info from $remote")
              requestDownload(
                maxModifiers,
                minHeadersPerBucket,
                maxHeadersPerBucket
              )(Option(getPeersForDownloadingHeaders(remote))) { howManyPerType =>
                Map(Header.modifierTypeId -> headerIds.reverse.filter(downloadRequired(historyReader)).take(howManyPerType))
              }
            }
          case Equal =>
            // does nothing for `Equal`
            log.debug(s"$remote has equal header-chain")
        }
      case _ =>
        // historyReader not initialized yet, it should not happen
        log.error("historyReader not initialized when processing syncInfo")
    }
  }

  /**
    * Headers should be downloaded from an Older node, it is triggered by received sync message from an older node
    * @param callingPeer that can be used to download headers, it must be Older
    * @return available peers to download headers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingHeaders(callingPeer: ConnectedPeer): (PeerSyncState, Iterable[ConnectedPeer]) = {
    statusTracker.peersByStatus
      .get(Older)
      .map(PeerSyncState.Older -> _)
      .getOrElse(PeerSyncState.OlderCalling -> Array(callingPeer))
  }

  /**
    * Other persistent modifiers besides headers should be downloaded from either Older or Equal node, with fallback to Unknown or Fork
    * @return available peers to download persistent modifiers from together with the state/origin of the peer
    */
  private def getPeersForDownloadingBlocks: Option[(PeerSyncState, Iterable[ConnectedPeer])] = {
    val peersByStatus = statusTracker.peersByStatus
    Option(peersByStatus.getOrElse(Older, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Equal, mutable.WrappedArray.empty))
      .filter(_.nonEmpty)
      .map(PeerSyncState.OlderOrEqual -> _)
      .orElse {
        Option(peersByStatus.getOrElse(Unknown, mutable.WrappedArray.empty) ++ peersByStatus.getOrElse(Fork, mutable.WrappedArray.empty))
          .filter(_.nonEmpty)
          .map(PeerSyncState.UnknownOrFork -> _)
      }
  }

  /**
    * Modifier download method that is given min/max constraints for modifiers to download from peers.
    * It sends requests for modifiers to given peers in optimally sized batches.
    * @param maxModifiers maximum modifiers to download
    * @param minModifiersPerBucket minimum modifiers to download per bucket
    * @param maxModifiersPerBucket maximum modifiers to download per bucket
    * @param getPeersOpt optionally get peers to download from, all peers have the same PeerSyncState
    * @param fetchMax function that fetches modifiers, it is passed how many of them tops
    */
  protected def requestDownload(maxModifiers: Int, minModifiersPerBucket: Int, maxModifiersPerBucket: Int)
                               (getPeersOpt: => Option[(PeerSyncState, Iterable[ConnectedPeer])])
                               (fetchMax: Int => Map[ModifierTypeId, Seq[ModifierId]]): Unit =
    getPeersOpt
      .foreach { case (peerStatus, peers) =>
        val modifiersByBucket = ElementPartitioner.distribute(peers, maxModifiers, minModifiersPerBucket, maxModifiersPerBucket)(fetchMax)
        // collect and log useful downloading progress information, don't worry it does not run frequently
        modifiersByBucket.headOption.foreach { _ =>
          modifiersByBucket
            .groupBy(_._1._2)
            .mapValues(_.map(_._2.size))
            .map { case (modType, batchSizes) =>
              s"Downloading from $peerStatus peers : type[$modType] of ${batchSizes.size} batches each of ~ size: ${batchSizes.take(2).max}"
            }.foreach(log.info(_))
        }
        // bucket represents a peer and a modifierType as we cannot send mixed types to a peer
        modifiersByBucket.foreach { case ((peer, modifierTypeId), modifierIds) =>
          deliveryTracker.setRequested(modifierIds, modifierTypeId, Some(peer))
          val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
        }
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
        if (valid.nonEmpty) viewHolderRef ! ModifiersFromRemote[ErgoPersistentModifier](valid)
      case _ =>
        log.error(s"Undefined serializer for modifier of type $typeId")
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
      if (typeId == Transaction.ModifierTypeId) {
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

  /** Alternative Peer Status dedicated only for peer syncing */
  sealed trait PeerSyncState
  object PeerSyncState {
    /** Peer for downloading headers must be older */
    case object Older extends PeerSyncState
    /** Peer downloading blocks can be older or equal */
    case object OlderOrEqual extends PeerSyncState
    /** Calling peer is always older */
    case object OlderCalling extends PeerSyncState
    /** Better Unknown or Fork than no peers */
    case object UnknownOrFork extends PeerSyncState
  }

}
