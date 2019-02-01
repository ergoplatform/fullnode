package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoPropertyTest {

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read(None)
    settings.directory shouldBe System.getProperty("user.dir") + "/ergo/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read(None)
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = 1000,
      PoPoWBootstrap = false,
      minimalSuffix = 10,
      mining = false,
      miningDelay = 1.second,
      offlineGeneration = false,
      keepVersions = 200,
      mempoolCapacity = 100000,
      blacklistCapacity = 100000,
      snapshotCreationInterval = 10000,
      keepLastSnapshots = 0
    )
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.json"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = 12,
      PoPoWBootstrap = false,
      minimalSuffix = 10,
      mining = false,
      miningDelay = 1.second,
      offlineGeneration = false,
      keepVersions = 200,
      mempoolCapacity = 100000,
      blacklistCapacity = 100000,
      snapshotCreationInterval = 10000,
      keepLastSnapshots = 0
    )
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.conf"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      blocksToKeep = 13,
      PoPoWBootstrap = false,
      minimalSuffix = 10,
      mining = false,
      miningDelay = 1.second,
      offlineGeneration = false,
      keepVersions = 200,
      mempoolCapacity = 100000,
      blacklistCapacity = 100000,
      snapshotCreationInterval = 10000,
      keepLastSnapshots = 0
    )
  }

}
