package org.ergoplatform.bench.misc

import java.net.URL

import javax.net.ssl.HttpsURLConnection
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder.CurrentView
import scorex.util.ScorexLogging

object Utils extends ScorexLogging {

  /** Elapsed time of `block` execution in milliseconds */
  def elapsedTimeOf[R](block: => R): Long = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    (t1 - t0) / 1000000
  }

  def adjustView(v: CurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool]): Unit = {
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

  def readModifiers(fileName: String, threshold: Int): Vector[ErgoPersistentModifier] = {
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
