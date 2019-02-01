package org.ergoplatform.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.ergoplatform.ErgoApp
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.state.StateType.Digest
import scorex.core.settings.{ScorexSettings, SettingsReaders}
import scorex.util.ScorexLogging

import scala.collection.JavaConverters._

case class ErgoSettings(directory: String,
                        chainSettings: ChainSettings,
                        testingSettings: TestingSettings,
                        nodeSettings: NodeConfigurationSettings,
                        scorexSettings: ScorexSettings,
                        walletSettings: WalletSettings,
                        cacheSettings: CacheSettings,
                        votingTargets: Map[Byte, Int] = Map()) {
  lazy val emission = new EmissionRules(chainSettings.monetary)
}

object ErgoSettings extends ScorexLogging
  with PowSchemeReaders
  with NodeConfigurationReaders
  with SettingsReaders {

  val configPath: String = "ergo"
  val scorexConfigPath: String = "scorex"

  def read(userConfigPath: Option[String]): ErgoSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  def fromConfig(config: Config): ErgoSettings = {
    val directory = config.as[String](s"$configPath.directory")

    val nodeSettings = config.as[NodeConfigurationSettings](s"$configPath.node")
    val chainSettings = config.as[ChainSettings](s"$configPath.chain")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")
    val walletSettings = config.as[WalletSettings](s"$configPath.wallet")
    val cacheSettings = config.as[CacheSettings](s"$configPath.cache")
    val scorexSettings = config.as[ScorexSettings](scorexConfigPath)

    val votingSettings = config.getObject(s"$configPath.voting")
    val votingTargets = votingSettings.keySet().asScala.map{id =>
      id.toInt.toByte -> votingSettings.get(id).render().toInt
    }.toMap


    if (nodeSettings.stateType == Digest && nodeSettings.mining) {
      log.error("Malformed configuration file was provided! Mining is not possible with digest state. Aborting!")
      ErgoApp.forceStopApplication()
    }

    consistentSettings(
      ErgoSettings(directory, chainSettings, testingSettings, nodeSettings, scorexSettings, walletSettings, cacheSettings, votingTargets)
    )
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("ergo")) failWithError("`ergo` path missed")
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }
  }

  private def consistentSettings(settings: ErgoSettings): ErgoSettings = {
    if (settings.nodeSettings.keepVersions < 0) {
      failWithError("nodeSettings.keepVersions should not be negative")
    } else if (!settings.nodeSettings.verifyTransactions && !settings.nodeSettings.stateType.requireProofs) {
      failWithError("Can not use UTXO state when nodeSettings.verifyTransactions is false")
    } else if (settings.nodeSettings.snapshotCreationInterval <= 0) {
      failWithError("settings.nodeSettings.snapshotCreationInterval should be positive")
    } else {
      settings
    }
  }

  private def failWithError(msg: String): Nothing = {
    log.error(s"Stop application due to malformed configuration file: $msg")
    ErgoApp.forceStopApplication()
  }
}
