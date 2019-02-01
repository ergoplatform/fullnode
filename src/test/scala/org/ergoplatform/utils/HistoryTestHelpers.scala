package org.ergoplatform.utils

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.modifierprocessors.EmptyBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.scalacheck.Gen
import scorex.core.settings.ScorexSettings

import scala.concurrent.duration._

trait HistoryTestHelpers extends ErgoPropertyTest {

  override lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep: Int = BlocksInChain + 1

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.headersHeight
    if (historyHeight < height) {
      history match {
        case _: EmptyBlockSectionProcessor =>
          val chain = genHeaderChain(height - historyHeight, history)
          if (history.isEmpty) applyHeaderChain(history, chain) else applyHeaderChain(history, chain.tail)
        case _ =>
          ???
      }
    } else {
      history
    }
  }

  def generateHistory(verifyTransactions: Boolean,
                      stateType: StateType,
                      PoPoWBootstrap: Boolean,
                      blocksToKeep: Int,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val protocolVersion = 1: Byte
    val networkPrefix = 0: Byte
    val blockInterval = 1.minute
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(
      stateType,
      verifyTransactions,
      blocksToKeep,
      PoPoWBootstrap,
      minimalSuffix,
      mining = false,
      miningDelay = 1.second,
      offlineGeneration = false,
      keepVersions = 200,
      mempoolCapacity = 100000,
      blacklistCapacity = 100000,
      snapshotCreationInterval = 10000,
      keepLastSnapshots = 2
    )
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = ChainSettings(protocolVersion, networkPrefix, blockInterval, epochLength, useLastEpochs,
      votingSettings, powScheme, settings.chainSettings.monetary)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

}
