package org.ergoplatform.utils

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.StatusReply
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.{AutolykosSolution, ErgoMiner, WorkMessage}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{DigestState, ErgoStateContext, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.sanity.ErgoSanity.HT
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.settings._
import org.ergoplatform.utils.generators.{ChainGenerator, ErgoGenerators, ErgoTransactionGenerators}
import org.ergoplatform.wallet.boxes.{ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.P2PKAddress
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalacheck.Gen
import scorex.core.app.Version
import scorex.core.network.NetworkController.ReceivableMessages.GetConnectedPeers
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}
import scorex.core.network.{Handshake, PeerSpec}
import scorex.core.settings.ScorexSettings
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.db.ByteArrayWrapper
import scorex.util.Random
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait Stubs extends ErgoGenerators with ErgoTestHelpers with ChainGenerator with scorex.testkit.utils.FileUtils {

  implicit val system: ActorSystem

  val chain: Seq[ErgoFullBlock] = genChain(6)

  val history: HT = applyChain(generateHistory(), chain)

  val digestState: DigestState = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, settings)).map { wus =>
      DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
    }
  }.sample.value

  val utxoSettings: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(stateType = StateType.Utxo))

  val utxoState: WrappedUtxoState =
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, utxoSettings)).sample.value

  lazy val wallet = new WalletStub

  val txs: Seq[ErgoTransaction] = validTransactionsFromBoxHolder(boxesHolderGen.sample.get)._1
  val memPool: ErgoMemPool = ErgoMemPool.empty(settings).put(txs).get

  val digestReaders = Readers(history, digestState, memPool, wallet)

  val utxoReaders = Readers(history, utxoState, memPool, wallet)

  val protocolVersion = Version("1.1.1")

  val peerSpec: PeerSpec = defaultPeerSpec.copy(protocolVersion = protocolVersion)

  val connectedPeers: Seq[Handshake] = Seq(
    Handshake(peerSpec.copy(nodeName = "first"), ts1),
    Handshake(peerSpec.copy(nodeName = "second"), ts2)
  )

  val blacklistedPeers: Seq[String] = Seq("4.4.4.4:1111", "8.8.8.8:2222")

  val pk: ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray(Random.randomBytes(32))).publicImage
  val externalCandidateBlock = WorkMessage(Array.fill(32)(2: Byte), BigInt(9999), None, pk, None)

  class PeersManagerStub extends Actor {
    def receive: Receive = {
      case GetAllPeers => sender() ! peers
      case GetBlacklistedPeers => sender() ! blacklistedPeers
    }
  }

  object PeersManagerStub {
    def props(): Props = Props(new PeersManagerStub)
  }

  class MinerStub extends Actor {
    def receive: Receive = {
      case ErgoMiner.PrepareCandidate(_, reply) => if (reply) {
        sender() ! StatusReply.success(externalCandidateBlock)
      }
      case _: AutolykosSolution => sender() ! StatusReply.success(())
      case ErgoMiner.ReadMinerPk => sender() ! Some(pk)
    }
  }

  object MinerStub {
    def props(): Props = Props(new MinerStub)
  }

  class NodeViewStub extends Actor {
    def receive: Receive = {
      case _ =>
    }
  }

  object NodeViewStub {
    def props(): Props = Props(new NodeViewStub)
  }

  class NetworkControllerStub extends Actor {
    def receive: Receive = {
      case GetConnectedPeers => sender() ! connectedPeers
      case _ =>
    }
  }

  object NetworkControllerStub {
    def props(): Props = Props(new NetworkControllerStub)
  }

  class PeerManagerStub extends Actor {
    def receive: Receive = {
      case _ =>
    }
  }

  object PeerManagerStub {
    def props(): Props = Props(new PeerManagerStub)
  }

  class WalletActorStub extends Actor {

    import WalletActorStub.{walletBoxN_N, walletBox10_10, walletBox20_30, walletBoxSpent21_31, walletTxs}

    private val prover: ErgoProvingInterpreter = defaultProver
    private val trackedAddresses: Seq[P2PKAddress] = prover.hdPubKeys.map(epk => P2PKAddress(epk.key))

    private val apps = mutable.Map[ScanId, Scan]()

    private val ergoWalletService = new ErgoWalletServiceImpl

    def receive: Receive = {

      case _: InitWallet => sender() ! Success(WalletActorStub.mnemonic)

      case _: RestoreWallet => sender() ! Success(())

      case _: UnlockWallet => sender() ! Success(())

      case LockWallet => ()

      case RescanWallet => sender ! Success(())

      case GetWalletStatus => sender() ! WalletStatus(true, true, None, ErgoHistory.GenesisHeight)

      case _: CheckSeed => sender() ! true

      case GetWalletBoxes(unspentOnly, _) =>
        val boxes = if (unspentOnly) {
          Seq(walletBox10_10, walletBox20_30)
        } else {
          Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        }
        sender() ! boxes.sortBy(_.trackedBox.inclusionHeightOpt)

      case GetTransactions(_) =>
        sender() ! walletTxs

      case DeriveKey(_) => sender() ! Success(WalletActorStub.address)

      case DeriveNextKey => sender() !
        DeriveNextKeyResult(Success(WalletActorStub.path, WalletActorStub.address, WalletActorStub.secretKey))

      case ReadPublicKeys(from, until) =>
        sender() ! trackedAddresses.slice(from, until)

      case ReadBalances(chainStatus) =>
        sender() ! WalletDigest(0, WalletActorStub.balance(chainStatus), mutable.WrappedArray.empty)

      case AddScan(req) =>
        val scanId = ScanId @@ (apps.lastOption.map(_._1).getOrElse(100: Short) + 1).toShort
        val app = req.toScan(scanId)
        apps += scanId -> app.get
        sender() ! AddScanResponse(app)

      case RemoveScan(scanId) =>
        val res: Try[Unit] = if(apps.exists(_._1 == scanId)) {
          apps.remove(scanId)
          Success(())
        } else {
          Failure(new Exception(""))
        }
        sender() ! RemoveScanResponse(res)

      case GetScanBoxes(_, _, considerUnconfirmed) =>
        val res = if(considerUnconfirmed) {
          Seq(walletBoxN_N, walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        } else {
          Seq(walletBox10_10, walletBox20_30, walletBoxSpent21_31)
        }
        sender() ! res

      case StopTracking(scanId, boxId) =>
        sender() ! StopTrackingResponse(Success(()))

      case ReadScans =>
        sender() ! ReadScansResponse(apps.values.toSeq)

      case GenerateTransaction(_, _, _, _) =>
        val input = ErgoTransactionGenerators.inputGen.sample.value
        val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(ergoBoxCandidateGen.sample.value))
        sender() ! Success(tx)

      case SignTransaction(tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt) =>
        val sc = ErgoStateContext.empty(stateConstants)
        val params = LaunchParameters
        sender() ! ergoWalletService.signTransaction(Some(prover), tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt, params, sc) { boxId =>
          utxoState.versionedBoxHolder.get(ByteArrayWrapper(boxId))
        }
    }
  }

  object WalletActorStub {

    val seed: String = "walletstub"
    val mnemonic: String = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon"
    val path = DerivationPath(List(0, 1, 2), publicBranch = false)
    val secretKey = ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(mnemonic)).derive(path)
    val address = P2PKAddress(proveDlogGen.sample.get)

    val walletBoxN_N: WalletBox = WalletBox(
      TrackedBox(
        creationTxId = modifierIdGen.sample.get,
        creationOutIndex = 0,
        inclusionHeightOpt = None,
        spendingTxIdOpt = Some(modifierIdGen.sample.get),
        spendingHeightOpt = None,
        box = ergoBoxGen.sample.get,
        scans = Set(PaymentsScanId)
      ),
      confirmationsNumOpt = None
    )

    val walletBox10_10: WalletBox = WalletBox(
      TrackedBox(
        creationTxId = modifierIdGen.sample.get,
        creationOutIndex = 0,
        inclusionHeightOpt = Some(10),
        spendingTxIdOpt = Some(modifierIdGen.sample.get),
        spendingHeightOpt = None,
        box = ergoBoxGen.sample.get,
        scans = Set(PaymentsScanId)
      ),
      confirmationsNumOpt = Some(10)
    )
    val walletBox20_30: WalletBox = walletBox10_10.copy(
      confirmationsNumOpt = Some(20),
      trackedBox = walletBox10_10.trackedBox.copy(inclusionHeightOpt = Some(30))
    )
    val walletBoxSpent21_31: WalletBox = walletBox10_10.copy(
      confirmationsNumOpt = Some(21),
      trackedBox = walletBox10_10.trackedBox.copy(
        inclusionHeightOpt = Some(31),
        spendingHeightOpt = Some(32),
        spendingTxIdOpt = Some(modifierIdGen.sample.get)
      )
    )
    val walletTxs: Seq[AugWalletTransaction] =
      Gen.listOf(augWalletTransactionGen).sample.get

    def props(): Props = Props(new WalletActorStub)

    def balance(chainStatus: ChainStatus): Long = if (chainStatus.onChain) confirmedBalance else unconfirmedBalance

    def confirmedBalance: Long = 1L

    def unconfirmedBalance: Long = 2L
  }

  class WalletStub extends ErgoWalletReader {
    val walletActor: ActorRef = system.actorOf(WalletActorStub.props())
  }


  class DigestReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! digestReaders
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object DigestReadersStub {
    def props(): Props = Props(new DigestReadersStub)
  }

  class UtxoReadersStub extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case GetReaders => sender() ! utxoReaders
      case GetDataFromHistory(f) => sender() ! f(history)
    }
  }

  object UtxoReadersStub {
    def props(): Props = Props(new UtxoReadersStub)
  }


  lazy val digestReadersRef: ActorRef = system.actorOf(DigestReadersStub.props())
  lazy val utxoReadersRef: ActorRef = system.actorOf(UtxoReadersStub.props())

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

    val miningDelay = 1.second
    val minimalSuffix = 2
    val complexityLimit = initSettings.nodeSettings.maxTransactionComplexity
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, mining = false, complexityLimit, miningDelay, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 1.minute, rebroadcastCount = 200, 1000000, 100)
    val scorexSettings: ScorexSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, NetworkType.TestNet, chainSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

  def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.InitialVersion,
      settings.chainSettings.initialNBits,
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
