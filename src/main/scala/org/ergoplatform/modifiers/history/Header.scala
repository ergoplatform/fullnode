package org.ergoplatform.modifiers.history

import com.google.common.primitives._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosSolution, AutolykosSolutionSerializer}
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class Header(version: Version,
                  override val parentId: ModifierId,
                  interlinks: Seq[ModifierId],
                  ADProofsRoot: Digest32,
                  stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  transactionsRoot: Digest32,
                  timestamp: Timestamp,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  extensionRoot: Digest32,
                  powSolution: AutolykosSolution,
                  votes: Array[Byte], //3 bytes
                  override val sizeOpt: Option[Int] = None) extends PreHeader with ErgoPersistentModifier {

  override def serializedId: Array[Version] = Algos.hash(bytes)

  override type M = Header

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = BlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = BlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  def extensionId: ModifierId = BlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  override def minerPk: EcPointType = powSolution.pk

  lazy val sectionIds: Seq[(ModifierTypeId, ModifierId)] = Seq((ADProofs.modifierTypeId, ADProofsId),
    (BlockTransactions.modifierTypeId, transactionsId), (Extension.modifierTypeId, extensionId))

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight

  /**
    * Checks, that modifier m corresponds t this header
    */
  def isCorrespondingModifier(m: ErgoPersistentModifier): Boolean = sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this Header is new enough to possibly be the best header
    */
  def isNew(timeProvider: NetworkTimeProvider, timeDiff: FiniteDuration): Boolean = {
    timeProvider.time() - timestamp < timeDiff.toMillis
  }

  /**
    * New voting epoch starts
    */
  def votingStarts(votingEpochLength: Int): Boolean = height % votingEpochLength == 0 && height > 0

}

object Header extends ApiCodecs {

  val CurrentVersion: Byte = 1

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = bytesToId(Array.fill(Constants.HashLength)(0: Byte))

  implicit val jsonEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> Algos.encode(h.id).asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
      "interlinks" -> h.interlinks.map(i => Algos.encode(i).asJson).asJson,
      "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
      "stateRoot" -> Algos.encode(h.stateRoot).asJson,
      "parentId" -> Algos.encode(h.parentId).asJson,
      "timestamp" -> h.timestamp.asJson,
      "extensionHash" -> Algos.encode(h.extensionRoot).asJson,
      "powSolutions" -> h.powSolution.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "difficulty" -> h.requiredDifficulty.toString.asJson,
      "version" -> h.version.asJson,
      "votes" -> Algos.encode(h.votes).asJson,
      "size" -> h.size.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Header] = { c: HCursor =>
    for {
      transactionsRoot <- c.downField("transactionsRoot").as[Digest32]
      interlinks <- c.downField("interlinks").as[List[ModifierId]]
      adProofsRoot <- c.downField("adProofsRoot").as[Digest32]
      stateRoot <- c.downField("stateRoot").as[ADDigest]
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Long]
      extensionHash <- c.downField("extensionHash").as[Digest32]
      nBits <- c.downField("nBits").as[Long]
      height <- c.downField("height").as[Int]
      version <- c.downField("version").as[Byte]
      votes <- c.downField("votes").as[String]
      solutions <- c.downField("powSolutions").as[AutolykosSolution]
    } yield Header(version, parentId, interlinks, adProofsRoot, stateRoot,
      transactionsRoot, timestamp, nBits, height, extensionHash, solutions, Algos.decode(votes).get)
  }

}

object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinksAndPow(h: Header): Array[Byte] =
    Bytes.concat(
      Array(h.version),
      idToBytes(h.parentId),
      h.ADProofsRoot,
      h.transactionsRoot,
      h.stateRoot,
      Longs.toByteArray(h.timestamp),
      h.extensionRoot,
      RequiredDifficulty.toBytes(h.nBits),
      Ints.toByteArray(h.height),
      h.votes
    )

  def bytesWithoutPow(h: Header): Array[Byte] = {
    @SuppressWarnings(Array("TraversableHead"))
    def buildInterlinkBytes(links: Seq[ModifierId], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: ModifierId = links.head
        val repeatingInt = links.count(_ == headLink)
        val repeating: Byte = repeatingInt.toByte
        buildInterlinkBytes(links.drop(repeatingInt), Bytes.concat(acc, Array(repeating), idToBytes(headLink)))
      }
    }

    val interlinkBytes = buildInterlinkBytes(h.interlinks, Array[Byte]())
    val interlinkBytesSize = Chars.toByteArray(interlinkBytes.length.toChar)

    Bytes.concat(bytesWithoutInterlinksAndPow(h), interlinkBytesSize, interlinkBytes)
  }

  def bytesWithoutInterlinks(h: Header): Array[Byte] =
    Bytes.concat(
      bytesWithoutInterlinksAndPow(h),
      Chars.toByteArray(0),
      h.powSolution.bytes
    )

  override def toBytes(h: Header): Array[Version] =
    Bytes.concat(bytesWithoutPow(h), h.powSolution.bytes)

  @SuppressWarnings(Array("TryGet"))
  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytesToId(bytes.slice(1, 33))
    val ADProofsRoot = Digest32 @@ bytes.slice(33, 65)
    val transactionsRoot = Digest32 @@ bytes.slice(65, 97)
    val stateRoot = ADDigest @@ bytes.slice(97, 130)
    val timestamp = Longs.fromByteArray(bytes.slice(130, 138))
    val extensionHash = Digest32 @@ bytes.slice(138, 170)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(170, 174)).get
    val height = Ints.fromByteArray(bytes.slice(174, 178))
    val votes = bytes.slice(178, 181)

    @tailrec
    def parseInterlinks(index: Int, endIndex: Int, acc: Seq[ModifierId]): Seq[ModifierId] = if (endIndex > index) {
      val repeatN: Int = 0xff & bytes(index)
      require(repeatN != 0)
      val link: ModifierId = bytesToId(bytes.slice(index + 1, index + 33))
      val links: Seq[ModifierId] = Array.fill(repeatN)(link)
      parseInterlinks(index + 33, endIndex, acc ++ links)
    } else {
      acc
    }

    val interlinksSize = Chars.fromByteArray(bytes.slice(181, 183))
    val interlinks = parseInterlinks(183, 183 + interlinksSize, Seq.empty)

    val powSolutionsBytes = bytes.slice(183 + interlinksSize, bytes.length)

    AutolykosSolutionSerializer.parseBytes(powSolutionsBytes) map { powSolution =>
      Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
        nBits, height, extensionHash, powSolution, votes, Some(bytes.length))
    }
  }.flatten

}
