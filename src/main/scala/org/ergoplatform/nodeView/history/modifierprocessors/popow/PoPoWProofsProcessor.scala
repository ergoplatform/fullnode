package org.ergoplatform.nodeView.history.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{Header, HeaderChain, PoPoWProof}
import org.ergoplatform.nodeView.history.modifierprocessors.HeadersProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.util.Try

/**
  * Contains all functions required by History to process PoPoWProofs and generate them.
  */
trait PoPoWProofsProcessor extends HeadersProcessor with ScorexLogging {

  def validate(m: PoPoWProof): Try[Unit]

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier]

  def lastHeaders(count: Int, offset: Int = 0): HeaderChain

  /**
    * Constructs SPV Proof from KLS16 paper
    * TODO: replace with KMZ17" (or NiPoPoW)
    *
    * @param m - parameter "m" from the paper (minimal length of innerchain to include)
    * @param k - parameter "k" from the paper (chain suffix)
    * @return
    */
  def constructPoPoWProof(m: Int, k: Int): Try[PoPoWProof] = Try {
    val currentHeight = headersHeight
    require(m > 0 && m < currentHeight, s"$m > 0 && $m < $currentHeight")
    require(k > 0 && k < currentHeight, s"$k > 0 && $k < $currentHeight")

    val suffix: HeaderChain = lastHeaders(k)
    val suffixFirstHeader = suffix.head

    @tailrec
    def constructProof(depth: Int): (Int, Seq[Header]) = {
      require(depth >= 0)

      @tailrec
      def loop(acc: Seq[Header]): Seq[Header] = {
        val interHeader = acc.head
        if (interHeader.interlinks.lengthCompare(depth) > 0) {
          val header = typedModifierById[Header](interHeader.interlinks(depth)).get
          loop(header +: acc)
        } else {
          acc.dropRight(1)
        }
      }

      val innerchain = loop(Seq(suffixFirstHeader))

      //todo: this code below could reach depth = -1, then ArrayIndexOutOfBoundException
      if (innerchain.lengthCompare(m) >= 0) (depth, innerchain) else constructProof(depth - 1)
    }

    val (depth, innerchain) = constructProof(suffixFirstHeader.interlinks.length)

    PoPoWProof(m.toByte, k.toByte, depth.toByte, innerchain, suffix.headers)(powScheme)
  }
}
