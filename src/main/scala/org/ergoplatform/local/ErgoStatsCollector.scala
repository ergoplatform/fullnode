package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoStateReader, StateType}
import org.ergoplatform.settings.{Algos, ErgoSettings, LaunchParameters, Parameters}
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.network.peer.PeerInfo
import scorex.core.utils.NetworkTimeProvider
import scorex.util.{ModifierId, ScorexLogging}

import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class ErgoStatsCollector(readersHolder: ActorRef,
                         networkController: ActorRef,
                         settings: ErgoSettings,
                         timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    readersHolder ! GetReaders
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    context.system.scheduler.schedule(10.seconds, 10.seconds)(networkController ! GetConnectedPeers)(context.system.dispatcher)
  }

  private var nodeInfo = NodeInfo(
    settings.scorexSettings.network.nodeName,
    Version.VersionString,
    0,
    0,
    None,
    settings.nodeSettings.stateType,
    None,
    settings.nodeSettings.mining,
    None,
    None,
    None,
    None,
    timeProvider.time(),
    None,
    LaunchParameters)

  override def receive: Receive = onConnectedPeers orElse getInfo orElse onMempoolChanged orElse onStateChanged orElse
    onHistoryChanged orElse onSemanticallySuccessfulModification orElse init

  private def init: Receive = {
    case Readers(h, s, _, _) =>
      nodeInfo = nodeInfo.copy(
        bestFullBlockOpt = h.bestFullBlockOpt,
        bestHeaderOpt = h.bestHeaderOpt,
        headersScore = h.bestHeaderOpt.flatMap(m => h.scoreOf(m.id)),
        fullBlocksScore = h.bestFullBlockOpt.flatMap(m => h.scoreOf(m.id)),
        genesisBlockIdOpt = h.headerIdsAtHeight(ErgoHistory.GenesisHeight).headOption,
        stateRoot = Some(Algos.encode(s.rootHash)),
        stateVersion = Some(s.version.toString),
        parameters = s.stateContext.currentParameters
      )
  }

  private def getInfo: Receive = {
    case GetNodeInfo => sender() ! nodeInfo
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(p) =>
      nodeInfo = nodeInfo.copy(unconfirmedCount = p.size)
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      nodeInfo = nodeInfo.copy(parameters = s.stateContext.currentParameters)
  }

  private def onHistoryChanged: Receive = {
    case ChangedHistory(h: ErgoHistory@unchecked) if h.isInstanceOf[ErgoHistory] =>

      if (nodeInfo.genesisBlockIdOpt.isEmpty) {
        nodeInfo = nodeInfo.copy(genesisBlockIdOpt = h.headerIdsAtHeight(ErgoHistory.GenesisHeight).headOption)
      }

      nodeInfo = nodeInfo.copy(bestFullBlockOpt = h.bestFullBlockOpt,
        bestHeaderOpt = h.bestHeaderOpt,
        headersScore = h.bestHeaderOpt.flatMap(m => h.scoreOf(m.id)),
        fullBlocksScore = h.bestFullBlockOpt.flatMap(m => h.scoreOf(m.id))
      )
  }

  private def onConnectedPeers: Receive = {
    case peers: Seq[PeerInfo@unchecked] if peers.headOption.forall(_.isInstanceOf[PeerInfo]) =>
      nodeInfo = nodeInfo.copy(peersCount = peers.length)
  }

  def onSemanticallySuccessfulModification: Receive = {
    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) =>
      nodeInfo = nodeInfo.copy(stateRoot = Some(Algos.encode(fb.header.stateRoot)),
        stateVersion = Some(fb.encodedId))
  }

}

object ErgoStatsCollector {

  case object GetNodeInfo

  case class NodeInfo(nodeName: String,
                      appVersion: String,
                      unconfirmedCount: Int,
                      peersCount: Int,
                      stateRoot: Option[String],
                      stateType: StateType,
                      stateVersion: Option[String],
                      isMining: Boolean,
                      bestHeaderOpt: Option[Header],
                      headersScore: Option[BigInt],
                      bestFullBlockOpt: Option[ErgoFullBlock],
                      fullBlocksScore: Option[BigInt],
                      launchTime: Long,
                      genesisBlockIdOpt: Option[ModifierId],
                      parameters: Parameters)

  object NodeInfo extends ApiCodecs {
    implicit val paramsEncoder: Encoder[Parameters] = org.ergoplatform.settings.ParametersSerializer.jsonEncoder

    implicit val jsonEncoder: Encoder[NodeInfo] = (ni: NodeInfo) =>
      Map(
        "name" -> ni.nodeName.asJson,
        "appVersion" -> Version.VersionString.asJson,
        "headersHeight" -> ni.bestHeaderOpt.map(_.height).asJson,
        "fullHeight" -> ni.bestFullBlockOpt.map(_.header.height).asJson,
        "bestHeaderId" -> ni.bestHeaderOpt.map(_.encodedId).asJson,
        "bestFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.encodedId).asJson,
        "previousFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.parentId).map(Algos.encode).asJson,
        "difficulty" -> ni.bestFullBlockOpt.map(_.header.requiredDifficulty).map(difficultyEncoder.apply).asJson,
        "headersScore" -> ni.headersScore.map(difficultyEncoder.apply).asJson,
        "fullBlocksScore" -> ni.fullBlocksScore.map(difficultyEncoder.apply).asJson,
        "unconfirmedCount" -> ni.unconfirmedCount.asJson,
        "stateRoot" -> ni.stateRoot.asJson,
        "stateType" -> ni.stateType.stateTypeName.asJson,
        "stateVersion" -> ni.stateVersion.asJson,
        "isMining" -> ni.isMining.asJson,
        "peersCount" -> ni.peersCount.asJson,
        "launchTime" -> ni.launchTime.asJson,
        "genesisBlockId" -> ni.genesisBlockIdOpt.asJson,
        "parameters" -> ni.parameters.asJson
      ).asJson
  }

}

object ErgoStatsCollectorRef {
  def props(readersHolder: ActorRef,
            networkController: ActorRef,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoStatsCollector(readersHolder, networkController, settings, timeProvider))

  def apply(readersHolder: ActorRef, networkController: ActorRef, settings: ErgoSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(readersHolder, networkController, settings, timeProvider))

  def apply(name: String,
            readersHolder: ActorRef,
            networkController: ActorRef,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(readersHolder, networkController, settings, timeProvider), name)

}
