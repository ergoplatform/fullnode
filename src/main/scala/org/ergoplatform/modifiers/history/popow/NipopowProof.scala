package org.ergoplatform.modifiers.history.popow

import io.circe.{Decoder, Encoder}
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions.LongOps
import scala.language.implicitConversions


/**
  * A structure representing NiPoPow proof as a persistent modifier.
  *
  * For details, see the foundational paper:
  *
  * [KMZ17] Non-Interactive Proofs of Proof-of-Work https://eprint.iacr.org/2017/963.pdf
  *
  * @param m          - security parameter (min μ-level superchain length)
  * @param k          - security parameter (min suffix length, >= 1)
  * @param prefix     - proof prefix headers
  * @param suffixHead - first header of the suffix
  * @param suffixTail - tail of the proof suffix headers
  */
case class NipopowProof(popowAlgos: NipopowAlgos,
                        m: Int,
                        k: Int,
                        prefix: Seq[PoPowHeader],
                        suffixHead: PoPowHeader,
                        suffixTail: Seq[Header]) {

  def serializer: ScorexSerializer[NipopowProof] = new NipopowProofSerializer(popowAlgos)

  def headersChain: Seq[Header] = prefixHeaders ++ suffixHeaders

  def prefixHeaders: Seq[Header] = prefix.map(_.header)

  def suffixHeaders: Seq[Header] = suffixHead.header +: suffixTail

  def chainOfLevel(l: Int): Seq[PoPowHeader] = prefix.filter(x => popowAlgos.maxLevelOf(x.header) >= l)

  /**
    * Implementation of the ≥ algorithm from [KMZ17], see Algorithm 4
    *
    * @param that - PoPoW proof to compare with
    * @return whether this PoPoW proof is better than "that"
    */
  def isBetterThan(that: NipopowProof): Boolean = {
    if (this.isValid && that.isValid) {
      popowAlgos.lowestCommonAncestor(headersChain, that.headersChain)
        .map(h => headersChain.filter(_.height > h.height) -> that.headersChain.filter(_.height > h.height))
        .exists({ case (thisDivergingChain, thatDivergingChain) =>
          popowAlgos.bestArg(thisDivergingChain)(m) > popowAlgos.bestArg(thatDivergingChain)(m) })
    } else {
      this.isValid
    }
  }

  /**
    * Checks if the proof is valid: if the heights are consistent and the connections are valid.
    * @return true if the proof is valid
    */
  def isValid: Boolean = {
    this.hasValidConnections && this.hasValidHeights
  }

  /**
    * Checks if the heights of the header-chain provided are consistent, meaning that for any two blocks b1 and b2,
    * if b1 precedes b2 then b1's height should be smaller.
    *
    * @return true if the heights of the header-chain are consistent
    */
  def hasValidHeights: Boolean = {
    headersChain.zip(headersChain.tail).forall({
      case (prev, next) => prev.height < next.height
    })
  }

  /**
    * Checks the connections of the blocks in the proof. Adjacent blocks should be linked either via interlink
    * or parent block id.
    *
    * @return true if all adjacent blocks are correctly connected
    */
  def hasValidConnections: Boolean = {
    prefix.zip(prefix.tail :+ suffixHead).forall({
      case (prev, next) => next.interlinks.contains(prev.id)
    }) && (suffixHead.header +: suffixTail).zip(suffixTail).forall({
      case (prev, next) => next.parentId == prev.id
    })
  }
}

object NipopowProof {
  import io.circe.syntax._
  import PoPowHeader._

  implicit val nipopowProofEncoder: Encoder[NipopowProof] = { proof: NipopowProof =>
    Map(
      "m" -> proof.m.asJson,
      "k" -> proof.k.asJson,
      "prefix" -> proof.prefix.asJson,
      "suffixHead" -> proof.suffixHead.asJson,
      "suffixTail" -> proof.suffixTail.asJson
    ).asJson
  }

  def nipopowProofDecoder(poPowAlgos: NipopowAlgos): Decoder[NipopowProof] = { c =>
    for {
      m <- c.downField("m").as[Int]
      k <- c.downField("k").as[Int]
      prefix <- c.downField("prefix").as[Seq[PoPowHeader]]
      suffixHead <- c.downField("suffixHead").as[PoPowHeader]
      suffixTail <- c.downField("suffixTail").as[Seq[Header]]
    } yield NipopowProof(poPowAlgos, m, k, prefix, suffixHead, suffixTail)
  }

}

class NipopowProofSerializer(poPowAlgos: NipopowAlgos) extends ScorexSerializer[NipopowProof] {

  override def serialize(obj: NipopowProof, w: Writer): Unit = {
    w.putUInt(obj.m)
    w.putUInt(obj.k)
    w.putUInt(obj.prefix.size)
    obj.prefix.foreach { h =>
      val hBytes = h.bytes
      w.putUInt(hBytes.length)
      w.putBytes(hBytes)
    }
    val suffixHeadBytes = obj.suffixHead.bytes
    w.putUInt(suffixHeadBytes.length)
    w.putBytes(suffixHeadBytes)
    w.putUInt(obj.suffixTail.size)
    obj.suffixTail.foreach { h =>
      val hBytes = h.bytes
      w.putUInt(hBytes.length)
      w.putBytes(hBytes)
    }
  }

  override def parse(r: Reader): NipopowProof = {
    val m = r.getUInt().toIntExact
    val k = r.getUInt().toIntExact
    val prefixSize = r.getUInt().toIntExact
    val prefix = (0 until prefixSize).map { _ =>
      val size = r.getUInt().toIntExact
      PoPowHeaderSerializer.parseBytes(r.getBytes(size))
    }
    val suffixHeadSize = r.getUInt().toIntExact
    val suffixHead = PoPowHeaderSerializer.parseBytes(r.getBytes(suffixHeadSize))
    val suffixSize = r.getUInt().toIntExact
    val suffixTail = (0 until suffixSize).map { _ =>
      val size = r.getUInt().toIntExact
      HeaderSerializer.parseBytes(r.getBytes(size))
    }
    NipopowProof(poPowAlgos, m, k, prefix, suffixHead, suffixTail)
  }

}
