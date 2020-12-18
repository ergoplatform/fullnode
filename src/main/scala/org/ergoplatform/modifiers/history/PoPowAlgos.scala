package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.settings.Constants
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import Extension.InterlinksVectorPrefix
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32

/**
  * A set of utilities for working with PoPoW security protocol.
  */
object PoPowAlgos {

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  @inline def updateInterlinks(prevHeaderOpt: Option[Header], prevExtensionOpt: Option[Extension]): Seq[ModifierId] = {
    prevHeaderOpt
      .flatMap { h =>
        prevExtensionOpt
          .flatMap(ext => unpackInterlinks(ext.fields).toOption)
          .map(updateInterlinks(h, _))
      }
      .getOrElse(Seq.empty)
  }

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  @inline def updateInterlinks(prevHeader: Header, prevInterlinks: Seq[ModifierId]): Seq[ModifierId] = {
    if (!prevHeader.isGenesis) {
      require(prevInterlinks.nonEmpty, "Interlinks vector could not be empty in case of non-genesis header")
      val genesis = prevInterlinks.head
      val tail = prevInterlinks.tail
      val prevLevel = maxLevelOf(prevHeader) & 0xFF
      if (prevLevel > 0) {
        (genesis +: tail.dropRight(prevLevel)) ++ Seq.fill(prevLevel)(prevHeader.id)
      } else {
        prevInterlinks
      }
    } else {
      Seq(prevHeader.id)
    }
  }

  /**
    * Packs interlinks into extension key-value format.
    */
  @inline def packInterlinks(links: Seq[ModifierId]): Seq[(Array[Byte], Array[Byte])] = {
    def loop(rem: List[(ModifierId, Int)],
             acc: Seq[(Array[Byte], Array[Byte])]): Seq[(Array[Byte], Array[Byte])] = {
      rem match {
        case (headLink, idx) :: _ =>
          val duplicatesQty = links.count(_ == headLink)
          val filed = Array(InterlinksVectorPrefix, idx.toByte) -> (duplicatesQty.toByte +: idToBytes(headLink))
          loop(rem.drop(duplicatesQty), acc :+ filed)
        case Nil =>
          acc
      }
    }

    loop(links.zipWithIndex.toList, Seq.empty)
  }

  /**
    * Proves the inclusion of an interlink pointer to blockId in the Merkle Tree of the given extension.
    */
  def proofForInterlink(ext: ExtensionCandidate, blockId: ModifierId): Option[MerkleProof[Digest32]] = {
    ext.fields
      .find({ case (key, value) => key.head == InterlinksVectorPrefix && (value.tail sameElements idToBytes(blockId)) })
      .flatMap({ case (key, _) => ext.proofFor(key) })
  }

  @inline def interlinksToExtension(links: Seq[ModifierId]): ExtensionCandidate = {
    ExtensionCandidate(packInterlinks(links))
  }

  /**
    * Unpacks interlinks from key-value format of extension.
    */
  @inline def unpackInterlinks(fields: Seq[(Array[Byte], Array[Byte])]): Try[Seq[ModifierId]] = {
    @tailrec
    def loop(rem: List[(Array[Byte], Array[Byte])],
             acc: Seq[ModifierId] = Seq.empty): Try[Seq[ModifierId]] = {
      rem match {
        case head :: tail =>
          val value = head._2
          if (value.lengthCompare(Constants.ModifierIdSize + 1) == 0) {
            val duplicatesQty = 0xff & value.head.toInt
            val link = bytesToId(value.tail)
            loop(tail, acc ++ Seq.fill(duplicatesQty)(link))
          } else {
            Failure(new Exception("Interlinks improperly packed"))
          }
        case Nil =>
          Success(acc)
      }
    }

    loop(fields.filter(_._1.headOption.contains(InterlinksVectorPrefix)).toList)
  }

  /**
    * Computes max level (μ) of the given [[Header]], such that μ = log(T) − log(id(B))
    */
  private def maxLevelOf(header: Header): Int = {
    if (!header.isGenesis) {
      def log2(x: BigInt) : Double = {
        math.log(x.doubleValue()) / math.log(2)
      }

      val requiredTarget = org.ergoplatform.mining.q / RequiredDifficulty.decodeCompactBits(header.nBits)
      val realTarget = header.powSolution.d
      val level = log2(requiredTarget) - log2(realTarget)
      level.toInt
    } else {
      Int.MaxValue
    }
  }

}
