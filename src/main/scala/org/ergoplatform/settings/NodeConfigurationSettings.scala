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
                                     poPoWBootstrap: Boolean,
                                     minimalSuffix: Int,
                                     mining: Boolean,
                                     maxTransactionComplexity: Int,
                                     miningDelay: FiniteDuration,
                                     useExternalMiner: Boolean,
                                     miningPubKeyHex: Option[String],
                                     offlineGeneration: Boolean,
                                     keepVersions: Int,
                                     mempoolCapacity: Int,
                                     blacklistCapacity: Int,
                                     mempoolCleanupDuration: FiniteDuration,
                                     rebroadcastCount: Int,
                                     minimalFeeAmount: Long,
                                     headerChainDiff: Int,
                                     skipV1TransactionsValidation: Boolean = false,
                                     collectMetrics: Boolean = false) {
  /**
    * Whether the node keeping all the full blocks of the blockchain or not.
    * @return true if the blockchain is pruned, false if not
    */
  val isFullBlocksPruned: Boolean = blocksToKeep >= 0
}

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
      cfg.as[Int](s"$path.maxTransactionComplexity"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.useExternalMiner"),
      cfg.as[Option[String]](s"$path.miningPubKeyHex"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Int](s"$path.keepVersions"),
      cfg.as[Int](s"$path.mempoolCapacity"),
      cfg.as[Int](s"$path.blacklistCapacity"),
      cfg.as[FiniteDuration](s"$path.mempoolCleanupDuration"),
      cfg.as[Int](s"$path.rebroadcastCount"),
      cfg.as[Long](s"$path.minimalFeeAmount"),
      cfg.as[Int](s"$path.headerChainDiff"),
      cfg.as[Boolean](s"$path.skipV1TransactionsValidation"),
      cfg.as[Boolean](s"$path.collectMetrics")
    )
  }

}
