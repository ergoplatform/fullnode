package org.ergoplatform.it.container

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import org.ergoplatform.it.container.Docker.ExtraConfig
import org.ergoplatform.utils.ErgoTestConstants

import scala.collection.JavaConverters._

trait IntegrationTestConstants extends ErgoTestConstants {

  val defaultConfigTemplate: Config = ConfigFactory.parseResources("template.conf")
  val nodesJointConfig: Config = ConfigFactory.parseResources("nodes.conf").resolve()
  val nodeSeedConfigs: List[Config] = nodesJointConfig.getConfigList("nodes").asScala.toList

  def starTopologyConfig: ExtraConfig = { (docker, nodeConfig) =>
    docker.nodes.headOption collect {
      case node if node.nodeName != nodeNameFromConfig(nodeConfig) => knownPeersConfig(Seq(node.nodeInfo))
    }
  }

  def sequentialTopologyConfig: ExtraConfig = { (docker, nodeConfig) =>
    val nodeName = nodeNameFromConfig(nodeConfig)
    val previousNode = docker.nodes.takeWhile(_.nodeName != nodeName).lastOption
    previousNode map { node => knownPeersConfig(Seq(node.nodeInfo)) }
  }

  def isolatedPeersConfig: ExtraConfig = { (_, _) =>
    Some(knownPeersConfig(Seq.empty))
  }

  def nodeNameFromConfig(nodeConfig: Config): String = {
    nodeConfig.as[Option[String]]("scorex.network.nodeName").getOrElse("")
  }

  def specialDataDirConfig(dir: String): Config = ConfigFactory.parseString(
    s"""
       |ergo.directory=$dir
    """.stripMargin
  )

  def prunedHistoryConfig(blocksToKeep: Int): Config = ConfigFactory.parseString(
    s"""
       |ergo.node.blocksToKeep=$blocksToKeep
    """.stripMargin
  )

  def miningDelayConfig(millis: Int): Config = ConfigFactory.parseString(
    s"""
      |ergo.node.miningDelay=${millis}ms
    """.stripMargin
  )

  def snapshotCreationIntervalConfig(interval: Int): Config = ConfigFactory.parseString(
    s"""
       |ergo.node.snapshotCreationInterval=$interval
    """.stripMargin
  )

  def keepLastSnapshotsConfig(n: Int): Config = ConfigFactory.parseString(
    s"""
       |ergo.node.keepLastSnapshots=$n
    """.stripMargin
  )

  def blockIntervalConfig(millis: Int): Config = ConfigFactory.parseString(
    s"""
       |ergo.chain.blockInterval=${millis}ms
    """.stripMargin
  )

  val nonGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=false
    """.stripMargin
  )

  val onlineGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=false
    """.stripMargin
  )

  val offlineGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=true
    """.stripMargin
  )

  val shortMiningDelayConfig: Config = miningDelayConfig(500)

  val digestStatePeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.stateType = "digest"
    """.stripMargin
  )

  def knownPeersConfig(knownNodeInfo: Seq[NodeInfo]): Config = ConfigFactory.parseString({
    val peerInfoSeq = knownNodeInfo.map(n =>
      s"""
         |"${n.networkIpAddress}:${n.containerNetworkPort}"
       """.stripMargin)
    val peerInfoStr = peerInfoSeq.mkString("[", ",", "]")
    s"""
       |scorex.network.knownPeers=$peerInfoStr
     """.stripMargin
  })

  def declaredAddressConfig(ipAddress: String, port: Int): Config = ConfigFactory.parseString(
    s"""
       |scorex.network.declaredAddress="$ipAddress:$port"
     """.stripMargin
  )

}
