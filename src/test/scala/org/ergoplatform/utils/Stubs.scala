package org.ergoplatform.utils

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{GenerateTransaction, ReadBalances, ReadPublicKeys, ReadTrackedAddresses}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.sanity.ErgoSanity.HT
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings._
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators, ErgoTransactionGenerators}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.app.Version
import scorex.core.network.Handshake
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}
import scorex.core.settings.ScorexSettings
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils

import scala.concurrent.duration._
import scala.util.Success

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with FileUtils {

  implicit val system: ActorSystem

  lazy val chain: Seq[ErgoFullBlock] = genChain(4)

  lazy val history: HT = applyChain(generateHistory(), chain)

  lazy val state: DigestState = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, settings)).map { wus =>
      DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
    }
  }.sample.value

  lazy val wallet = new WalletStub

  val txs: Seq[ErgoTransaction] = validTransactionsFromBoxHolder(boxesHolderGen.sample.get)._1

  lazy val memPool: ErgoMemPool = ErgoMemPool.empty(settings).put(txs).get
  lazy val readers = Readers(history, state, memPool, wallet)

  val inetAddr1 = new InetSocketAddress("92.92.92.92", 27017)
  val inetAddr2 = new InetSocketAddress("93.93.93.93", 27017)

  val ts1: Long = System.currentTimeMillis() - 100
  val ts2: Long = System.currentTimeMillis() + 100

  val peers = Map(
    inetAddr1 -> PeerInfo(ts1, Some(inetAddr1), Some("first"), None, Seq.empty),
    inetAddr2 -> PeerInfo(ts2, Some(inetAddr2), Some("second"), None, Seq.empty)
  )

  val protocolVersion = Version("1.1.1")

  val connectedPeers: Seq[Handshake] = Seq(
    Handshake("node_pop", protocolVersion, "first", Some(inetAddr1), Seq(), ts1),
    Handshake("node_pop", protocolVersion, "second", Some(inetAddr2), Seq(), ts2)
  )

  val blacklistedPeers: Seq[String] = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  class PeersManagerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetAllPeers => sender() ! peers
      case GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props(): Props = Props(new PeersManagerStub)
  }

  val minerInfo = MiningStatusResponse(isMining = false, candidateBlock = None)

  class MinerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case MiningStatusRequest => sender() ! minerInfo
    }
  }

  object MinerStub {
    def props(): Props = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case _ =>
    }
  }

  object NodeViewStub {
    def props(): Props = Props(new NodeViewStub)
  }

  class NetworkControllerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetConnectedPeers => sender() ! connectedPeers
      case _ =>
    }
  }

  object NetworkControllerStub {
    def props(): Props = Props(new NetworkControllerStub)
  }

  class PeerManagerStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case _ =>
    }
  }

  object PeerManagerStub {
    def props(): Props = Props(new PeerManagerStub)
  }

  class WalletActorStub extends Actor {
    def seed: String = "walletstub"

    private implicit val addressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings.chainSettings.addressPrefix)
    private val prover = ErgoProvingInterpreter(seed, 2, parameters)
    private val trackedAddresses: Seq[P2PKAddress] = prover.dlogPubkeys.map(P2PKAddress.apply)

    def receive: Receive = {

      case ReadPublicKeys(from, until) =>
        sender() ! trackedAddresses.slice(from, until)

      case ReadBalances(chainStatus) =>
        sender ! BalancesSnapshot(0, WalletActorStub.balance(chainStatus), Map.empty)

      case ReadTrackedAddresses =>
        sender ! trackedAddresses

      case GenerateTransaction(_) =>
        val input = ErgoTransactionGenerators.inputGen.sample.value
        val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(ergoBoxCandidateGen.sample.value))
        sender ! Success(tx)
    }
  }

  object WalletActorStub {
    def props(): Props = Props(new WalletActorStub)
    def balance(chainStatus: ChainStatus): Long = if (chainStatus.onchain) confirmedBalance else unconfirmedBalance
    def confirmedBalance: Long = 1L
    def unconfirmedBalance: Long = 2L
  }

  class WalletStub extends ErgoWalletReader {
    val actor: ActorRef = system.actorOf(WalletActorStub.props())
  }


  class ReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! readers
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object ReadersStub {
    def props(): Props = Props(new ReadersStub)
  }

  lazy val readersRef: ActorRef = system.actorOf(ReadersStub.props())
  lazy val minerRef: ActorRef = system.actorOf(MinerStub.props())
  lazy val peerManagerRef: ActorRef = system.actorOf(PeerManagerStub.props())
  lazy val pmRef: ActorRef = system.actorOf(PeersManagerStub.props())
  lazy val nodeViewRef: ActorRef = system.actorOf(NodeViewStub.props())
  lazy val networkControllerRef: ActorRef = system.actorOf(NetworkControllerStub.props())

  def generateHistory(verifyTransactions: Boolean = true,
                      stateType: StateType = StateType.Digest,
                      PoPoWBootstrap: Boolean = false,
                      blocksToKeep: Int = 100,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val protocolVersion = 0: Byte
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
    val monetarySettings = settings.chainSettings.monetary
    val chainSettings = ChainSettings(protocolVersion, networkPrefix, blockInterval, epochLength, useLastEpochs,
                                      votingSettings, powScheme, monetarySettings)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

  def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.CurrentVersion,
      Constants.InitialNBits,
      ADDigest @@ Array.fill(HashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Math.max(timeProvider.time(), bestTimestamp),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).value
  }

}
