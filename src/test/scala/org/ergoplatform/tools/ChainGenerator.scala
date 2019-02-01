package org.ergoplatform.tools

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, P2PKAddress, _}
import scorex.util.ModifierId
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

/**
  * Application object for chain generation.
  * Takes 2 parameters: start timestamp and path to history folder.
  * Generate blocks starting from start timestamp and until current time with expected block interval
  * between them, to ensure that difficulty does not change.
  */
object ChainGenerator extends TestKit(ActorSystem()) with App with ErgoTestHelpers {

  implicit val ergoAddressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  val EmissionTxCost: Long = 20000
  val MinTxAmount: Long = 2000000
  val RewardDelay: Int = 720
  val MaxTxsPerBlock: Int = 10

  val prover = defaultProver
  val minerPk = prover.dlogPubkeys.head
  val selfAddressScript = P2PKAddress(minerPk).script
  val minerProp = ErgoState.rewardOutputScript(RewardDelay, minerPk)

  val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval = 2.minute

  val boxSelector: BoxSelector = DefaultBoxSelector

  val startTime = args.headOption.map(_.toLong).getOrElse(timeProvider.time - (blockInterval * 10).toMillis)
  val dir = if (args.length < 2) new File("/tmp/ergo/data") else new File(args(1))
  val txsSize: Int = if (args.length < 3) 100 * 1024 else args(2).toInt

  val miningDelay = 1.second
  val minimalSuffix = 2
  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(
    StateType.Utxo,
    verifyTransactions = true,
    blocksToKeep = -1,
    PoPoWBootstrap = false,
    minimalSuffix = minimalSuffix,
    mining = false,
    miningDelay = miningDelay,
    offlineGeneration = false,
    keepVersions = 200,
    mempoolCapacity = 100000,
    blacklistCapacity = 100000,
    snapshotCreationInterval = 10000,
    keepLastSnapshots = 2
  )
  val monetarySettings = settings.chainSettings.monetary.copy(
    minerRewardDelay = RewardDelay,
    afterGenesisStateDigestHex = "d801a0e4573d6993caa2eda7dc97aad2b4c8ed51ebb0afe0dd272c8d7e26d5fd01"
  )
  val chainSettings = ChainSettings(0: Byte, 0: Byte, blockInterval, 256, 8, votingSettings, pow, monetarySettings)
  val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, settings.testingSettings,
    nodeSettings, settings.scorexSettings, settings.walletSettings, CacheSettings.default)
  val stateDir = ErgoState.stateDir(fullHistorySettings)
  stateDir.mkdirs()

  val genesisBox = ErgoState.genesisEmissionBox(fullHistorySettings.emission)

  val votingEpochLength = votingSettings.votingLength
  val protocolVersion = fullHistorySettings.chainSettings.protocolVersion

  val history = ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  allowToApplyOldBlocks(history)
  val (state, _) = ErgoState.generateGenesisUtxoState(stateDir, StateConstants(None, fullHistorySettings), settings)
  log.info(s"Going to generate a chain at ${dir.getAbsoluteFile} starting from ${history.bestFullBlockOpt}")

  val chain = loop(state, None, None, Seq())
  log.info(s"Chain of length ${chain.length} generated")
  history.bestHeaderOpt shouldBe history.bestFullBlockOpt.map(_.header)
  history.bestFullBlockOpt.get.id shouldBe chain.last
  log.info("History was generated successfully")
  System.exit(0)

  private def loop(state: UtxoState,
                   initBox: Option[ErgoBox],
                   last: Option[Header],
                   acc: Seq[ModifierId]): Seq[ModifierId] = {
    val time: Long = last.map(_.timestamp + blockInterval.toMillis).getOrElse(startTime)
    if (time < timeProvider.time) {
      val (txs, lastOut) = genTransactions(last.map(_.height).getOrElse(ErgoHistory.GenesisHeight),
        initBox, state.stateContext)

      val candidate = genCandidate(prover.dlogPubkeys.head, last, time, txs, state)
      val block = proveCandidate(candidate.get)

      history.append(block.header).get
      block.blockSections.foreach(s => if (!history.contains(s)) history.append(s).get)

      val outToPassNext = if (last.isEmpty) {
        block.transactions.flatMap(_.outputs).find(_.proposition == minerProp)
      } else {
        lastOut
      }

      assert(outToPassNext.isDefined)

      log.info(
        s"Block ${block.id} with ${block.transactions.size} transactions at height ${block.header.height} generated")

      loop(state.applyModifier(block).get, outToPassNext, Some(block.header), acc :+ block.id)
    } else {
      acc
    }
  }

  private def genTransactions(height: Height,
                              inOpt: Option[ErgoBox],
                              ctx: ErgoStateContext): (Seq[ErgoTransaction], Option[ErgoBox]) = {
    inOpt
      .find { bx =>
        val canUnlock = (bx.creationHeight + RewardDelay <= height) || (bx.proposition != minerProp)
        canUnlock && bx.proposition != genesisBox.proposition && bx.value >= MinTxAmount
      }
      .map { input =>
        val qty = MaxTxsPerBlock
        val amount = input.value
        val outs = (0 until qty).map(_ => new ErgoBoxCandidate(amount, selfAddressScript, height))
        val x = outs
          .foldLeft(Seq.empty[ErgoTransaction], input) { case ((acc, in), out) =>
            val inputs = IndexedSeq(in)
            val unsignedTx = new UnsignedErgoTransaction(
              inputs.map(_.id).map(id => new UnsignedInput(id)),
              IndexedSeq(out)
            )

            prover.sign(unsignedTx, inputs, ctx)
              .fold(_ => acc -> in, tx => (acc :+ tx) -> unsignedTx.outputs.head)
          }
          ._1
        (x, Some(x.last.outputs.head))
      }
      .getOrElse(Seq.empty -> inOpt)
  }

  private def genCandidate(minerPk: ProveDlog,
                           lastHeaderOpt: Option[Header],
                           ts: Long,
                           txsFromPool: Seq[ErgoTransaction],
                           state: UtxoStateReader): Try[CandidateBlock] = Try {
    val stateContext = state.stateContext
    val nBits: Long = lastHeaderOpt
      .map(parent => history.requiredDifficultyAfter(parent))
      .map(d => RequiredDifficulty.encodeCompactBits(d))
      .getOrElse(Constants.InitialNBits)

    val emptyExtensionCandidate = ExtensionCandidate(Seq())
    val (extensionCandidate, votes: Array[Byte], version: Byte) = lastHeaderOpt.map { header =>
      val newHeight = header.height + 1
      val currentParams = stateContext.currentParameters
      val betterVersion = protocolVersion > header.version
      val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
        .map(h => h + votingSettings.votingLength * votingSettings.softForkEpochs)
      val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
      val forkOrdered = fullHistorySettings.votingTargets.getOrElse(Parameters.SoftFork, 0) != 0
      val voteForFork = betterVersion && forkOrdered && forkVotingAllowed

      if (newHeight % votingEpochLength == 0 && newHeight > 0) {
        val newParams = currentParams.update(newHeight, voteForFork, stateContext.votingData.epochVotes, votingSettings)
        (newParams.toExtensionCandidate(Seq()),
          newParams.suggestVotes(fullHistorySettings.votingTargets, voteForFork),
          newParams.blockVersion)
      } else {
        (emptyExtensionCandidate,
          currentParams.vote(fullHistorySettings.votingTargets, stateContext.votingData.epochVotes, voteForFork),
          currentParams.blockVersion)
      }
    }.getOrElse((emptyExtensionCandidate, Array(0: Byte, 0: Byte, 0: Byte), Header.CurrentVersion))

    val emissionTxOpt = ErgoMiner.collectEmission(state, minerPk, fullHistorySettings.emission)
    val txs = emissionTxOpt.toSeq ++ txsFromPool

    state.proofsForTransactions(txs).map { case (adProof, adDigest) =>
      CandidateBlock(lastHeaderOpt, version, nBits, adDigest, adProof, txs, ts, extensionCandidate, votes)
    }
  }.flatten

  @tailrec
  private def proveCandidate(candidate: CandidateBlock): ErgoFullBlock = {
    log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and timestamp ${candidate.timestamp}")

    pow.proveCandidate(candidate, prover.secrets.head.w) match {
      case Some(fb) => fb
      case _ =>
        val minerTag = scorex.utils.Random.randomBytes(Extension.FieldKeySize)
        proveCandidate(candidate.copy(extension = ExtensionCandidate(Seq(Array(0: Byte, 2: Byte) -> minerTag))))
    }
  }

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  private def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
