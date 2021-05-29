package org.ergoplatform

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.stream.SystemMaterializer
import org.ergoplatform.http._
import org.ergoplatform.mining.ErgoMiner.StartMining
import org.ergoplatform.http.api.{ScanApiRoute, _}
import org.ergoplatform.local._
import org.ergoplatform.mining.ErgoMinerRef
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ModeFeature}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Args, ErgoSettings, NetworkType}
import scorex.core.api.http._
import scorex.core.app.{Application, ScorexContext}
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.network.{NetworkControllerRef, PeerFeature, PeerSynchronizerRef, UPnP, UPnPGateway}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.io.Source
import scala.util.{Failure, Success}

class ErgoApp(args: Args) extends ScorexLogging {

  log.info(s"Running with args: $args")

  private val ergoSettings: ErgoSettings = ErgoSettings.read(args)

  require(ergoSettings.scorexSettings.restApi.apiKeyHash.isDefined, "API key hash must be set")

  log.info(s"Working directory: ${ergoSettings.directory}")
  log.info(s"Secret directory: ${ergoSettings.walletSettings.secretStorage.secretDir}")

  implicit private def settings: ScorexSettings = ergoSettings.scorexSettings

  implicit private val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit private val executionContext: ExecutionContext = actorSystem.dispatcher

  private val features: Seq[PeerFeature] = Seq(ModeFeature(ergoSettings.nodeSettings))
  private val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap

  private val timeProvider = new NetworkTimeProvider(settings.ntp)

  private val upnpGateway: Option[UPnPGateway] =
    if (settings.network.upnpEnabled) UPnP.getValidGateway(settings.network) else None
  upnpGateway.foreach(_.addPort(settings.network.bindAddress.getPort))

  //an address to send to peers
  private val externalSocketAddress: Option[InetSocketAddress] =
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }

  private val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  private val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)

  private val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  private val peerManagerRef = PeerManagerRef(settings, scorexContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", settings.network, peerManagerRef, scorexContext)

  private val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  private val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  // Create an instance of ErgoMiner actor if "mining = true" in config
  private val minerRefOpt: Option[ActorRef] =
    if (ergoSettings.nodeSettings.mining) {
      Some(ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider))
    } else {
      None
    }

  private val statsCollectorRef: ActorRef =
    ErgoStatsCollectorRef(readersHolderRef, networkControllerRef, ergoSettings, timeProvider)

  private val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, ErgoSyncInfoMessageSpec,
      ergoSettings, timeProvider)

  // Launching PeerSynchronizer actor which is then registering itself at network controller
  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer",
    networkControllerRef, peerManagerRef, settings.network, featureSerializers)

  private val apiRoutes: Seq[ApiRoute] = Seq(
    EmissionApiRoute(ergoSettings),
    ErgoUtilsApiRoute(ergoSettings),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi),
    InfoApiRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    NipopowApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi),
    WalletApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings),
    UtxoApiRoute(readersHolderRef, settings.restApi),
    ScriptApiRoute(readersHolderRef, ergoSettings),
    ScanApiRoute(readersHolderRef, ergoSettings),
    NodeApiRoute(ergoSettings)
  ) ++ minerRefOpt.map(minerRef => MiningApiRoute(minerRef, ergoSettings)).toSeq


  private val swaggerRoute = SwaggerRoute(settings.restApi, swaggerConfig)
  private val panelRoute = NodePanelRoute()

  private val httpService = ErgoHttpService(apiRoutes, swaggerRoute, panelRoute)

  // Run mining immediately, i.e. without syncing if mining = true and offlineGeneration = true
  // Useful for local blockchains (devnet)
  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    require(minerRefOpt.isDefined, "Miner does not exist but mining = true in config")
    log.info(s"Starting mining with offlineGeneration")
    minerRefOpt.get ! StartMining
  }

  private val actorsToStop: Seq[ActorRef] = Seq(
    peerManagerRef,
    networkControllerRef,
    readersHolderRef,
    nodeViewSynchronizer,
    statsCollectorRef,
    nodeViewHolderRef
  ) ++ minerRefOpt.toSeq

  sys.addShutdownHook(ErgoApp.shutdown(actorSystem, actorsToStop))

  if (!ergoSettings.nodeSettings.stateType.requireProofs) {
    MempoolAuditorRef(nodeViewHolderRef, networkControllerRef, ergoSettings)
  }

  private def swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  private def run(): Unit = {
    require(settings.network.agentName.length <= Application.ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val mat: SystemMaterializer = SystemMaterializer.get(actorSystem)
    val bindAddress = settings.restApi.bindAddress

    Http().newServerAt(bindAddress.getAddress.getHostAddress, bindAddress.getPort).bindFlow(httpService.compositeRoute)

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  private def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    upnpGateway.foreach(_.deletePort(settings.network.bindAddress.getPort))
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

}

object ErgoApp extends ScorexLogging {

  val argParser = new scopt.OptionParser[Args]("ergo") {
      opt[String]("config")
        .abbr("c")
        .action((x, c) => c.copy(userConfigPathOpt = Some(x)))
        .text("location of ergo node configuration")
        .optional()
      opt[Unit]("devnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.DevNet)))
        .text("set network to devnet")
        .optional()
      opt[Unit]("testnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.TestNet)))
        .text("set network to testnet")
        .optional()
      opt[Unit]("mainnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.MainNet)))
        .text("set network to mainnet")
        .optional()
      help("help").text("prints this usage text")
  }

  def main(args: Array[String]): Unit = argParser.parse(args, Args()) match {
    case Some(argsParsed) => new ErgoApp(argsParsed).run()
    case None => // Error message will be displayed when arguments are bad
  }

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }

}
