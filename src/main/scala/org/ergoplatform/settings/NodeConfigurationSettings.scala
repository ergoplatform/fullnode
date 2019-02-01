package org.ergoplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.nodeView.state.StateType

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo node regime
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class NodeConfigurationSettings(stateType: StateType,
                                     verifyTransactions: Boolean,
                                     blocksToKeep: Int,
                                     PoPoWBootstrap: Boolean,
                                     minimalSuffix: Int,
                                     mining: Boolean,
                                     miningDelay: FiniteDuration,
                                     offlineGeneration: Boolean,
                                     keepVersions: Int,
                                     mempoolCapacity: Int,
                                     blacklistCapacity: Int,
                                     snapshotCreationInterval: Int,
                                     keepLastSnapshots: Int)

trait NodeConfigurationReaders extends StateTypeReaders with ModifierIdReader {

  implicit val nodeConfigurationReader: ValueReader[NodeConfigurationSettings] = { (cfg, path) =>
    val stateTypeKey = s"$path.stateType"
    val stateType = stateTypeFromString(cfg.as[String](stateTypeKey), stateTypeKey)
    NodeConfigurationSettings(
      stateType,
      cfg.as[Boolean](s"$path.verifyTransactions"),
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Boolean](s"$path.PoPoWBootstrap"),
      cfg.as[Int](s"$path.minimalSuffix"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Int](s"$path.keepVersions"),
      cfg.as[Int](s"$path.mempoolCapacity"),
      cfg.as[Int](s"$path.blacklistCapacity"),
      cfg.as[Int](s"$path.snapshotCreationInterval"),
      cfg.as[Int](s"$path.keepLastSnapshots")
    )
  }

}
