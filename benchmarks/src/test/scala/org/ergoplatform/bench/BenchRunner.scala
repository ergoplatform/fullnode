package org.ergoplatform.bench

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}
import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.bench.misc.{ModifierWriter, TempDir}
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.utils.{NetworkTimeProvider, NetworkTimeProviderSettings}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

object BenchRunner extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {
    new File(targetDirectory).mkdirs()
    val threshold = args.headOption.getOrElse("1000").toInt
    val fileName = args.lift(1).get
    val isUtxo = args.lift(2).isEmpty
    val state = if (isUtxo) StateType.Utxo else StateType.Digest
    val benchRef = BenchActor(threshold, state)
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    val settings = ErgoSettings.read(None)
    val nodeSettings = settings.nodeSettings.copy(stateType = state)

    lazy val ergoSettings: ErgoSettings = settings
      .copy(directory =  userDir.getAbsolutePath, nodeSettings = nodeSettings)

    log.info(s"Setting that being used:")
    log.info(s"$ergoSettings")

    val ce = new EmissionRules(ergoSettings.chainSettings.monetary)
    val ntpSettings = NetworkTimeProviderSettings("pool.ntp.org", 30 minutes, 30 seconds)
    val timeProvider = new NetworkTimeProvider(ntpSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    /**
      * It's a hack to set minimalFullBlockHeightVar to 0 and to avoid "Header Is Not Synced" error, cause
      * in our case we are considering only locally pre-generated modifiers.
      */
    nodeViewHolderRef ! GetDataFromCurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool, Unit](adjust)

    log.info("Starting to read modifiers.")
    val modifiers = readModifiers(fileName, threshold)
    log.info("Finished read modifiers, starting to bench.")
    log.info(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, modifiers)
  }

  private def adjust(v: CurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool]): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(v.history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true: Boolean)
    ()
  }

  private def readModifiers(fileName: String, threshold: Int): Vector[ErgoPersistentModifier] = {
    var counter = 0
    var headersQty = 0
    log.info("Start reading modifiers from data file.")
    val is = getUrlInputStream(fileName)
    val result = Stream
      .continually {
        counter += 1
        if (counter % 100 == 0) log.info(s"Already read $headersQty blocks.")
        val mod: Option[ErgoPersistentModifier] = ModifierWriter.read(is)
        if (mod.exists(_.modifierTypeId == Header.modifierTypeId)) headersQty += 1
        mod
      }
      .takeWhile(m => (headersQty <= threshold) && m.isDefined)
      .flatten
      .toVector

    log.info(s"Total modifiers: ${result.length}")

    result
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ErgoPersistentModifier]): Unit = {
    benchRef ! BenchActor.Start
    modifiers.foreach { m => nodeRef ! LocallyGeneratedModifier(m) }
  }

  private def getUrlInputStream(url: String,
                                connectTimeout: Int = 5000,
                                readTimeout: Int = 5000,
                                requestMethod: String = "GET") = {
    val u = new URL(url)
    val conn = u.openConnection.asInstanceOf[HttpsURLConnection]
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod(requestMethod)
    conn.connect()
    conn.getInputStream
  }

}
