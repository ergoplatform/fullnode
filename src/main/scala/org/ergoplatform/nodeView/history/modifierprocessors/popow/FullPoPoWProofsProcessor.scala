package org.ergoplatform.nodeView.history.modifierprocessors.popow

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{PoPoWProof, PoPoWProofUtils}
import org.ergoplatform.nodeView.history.ErgoHistory.GenesisHeight
import org.ergoplatform.nodeView.history.modifierprocessors.HeadersProcessor
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Success, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that accept them.
  */
trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor with HeadersProcessor {

  @SuppressWarnings(Array("TraversableHead"))
  def validate(m: PoPoWProof): Try[Unit] = new PoPoWProofUtils(powScheme).validate(m).map { _ =>
    bestHeaderIdOpt match {
      case Some(genesisId) =>
        heightOf(genesisId) match {
          case Some(GenesisHeight) =>
            if (!(m.suffix ++ m.innerchain).forall(_.interlinks.head == genesisId)) {
              Failure(new Error(s"Genesis id is incorrect for $m"))
            } else {
              Success(Unit)
            }
          case height =>
            //TODO what if we trying to apply better popow proof to non-empty history?
            Failure(new Error(s"Trying to apply PoPoW proof to history with height $height"))
        }
      case None =>
        Failure(new Error("Trying to apply PoPoW proof to history with unknown genesis"))
    }
  }

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier] = {
    val headers = m.innerchain ++ m.suffix
    val bestHeader = m.suffix.last
    val headersIndexes: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = headers.zipWithIndex.flatMap { case (h, i) =>
      val requiredDifficulty: BigInt = h.requiredDifficulty
      val headerHeight: Int = h.height
      Seq((headerHeightKey(h.id), ByteArrayWrapper(Ints.toByteArray(headerHeight))),
        //TODO howto?
        (headerScoreKey(h.id), ByteArrayWrapper((requiredDifficulty * (1 + i)).toByteArray)))
    }
    val bestHeaderRow = (BestHeaderKey, Algos.idToBAW(bestHeader.id))
    historyStorage.insert(Algos.idToBAW(bestHeader.id), bestHeaderRow +: headersIndexes, headers)

    ProgressInfo(None, toRemove = Seq.empty, toApply = Seq(m.suffix.last), toDownload = Seq.empty)
  }

}
