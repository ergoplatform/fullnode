package org.ergoplatform.modifiers.history.popow

import io.circe.{Decoder, Encoder}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

/**
  * Block header along with unpacked interlinks
  *
  * Interlinks are stored in reverse order: first element is always genesis header, then level of lowest target met etc
  *
  * Not used in the consensus protocol
  *
  */
case class PoPowHeader(header: Header, interlinks: Seq[ModifierId]) extends BytesSerializable {

  override type M = PoPowHeader

  override def serializer: ScorexSerializer[M] = PoPowHeaderSerializer

  def id: ModifierId = header.id

  def height: Int = header.height

}

object PoPowHeader {

  import io.circe.syntax._

  def fromBlock(b: ErgoFullBlock): Try[PoPowHeader] = {
    NipopowAlgos.unpackInterlinks(b.extension.fields).map { interlinkVector =>
      PoPowHeader(b.header, interlinkVector)
    }
  }

  implicit val interlinksEncoder: Encoder[Seq[ModifierId]] = { interlinksVector: Seq[ModifierId] =>
    interlinksVector.map(id => id: String).asJson
  }

  implicit val popowHeaderJsonEncoder: Encoder[PoPowHeader] = { p: PoPowHeader =>
    Map(
      "header" -> p.header.asJson,
      //order in JSON array is preserved according to RFC 7159
      "interlinks" -> p.interlinks.asJson
    ).asJson
  }

  implicit val popowHeaderJsonDecoder: Decoder[PoPowHeader] = { c =>
    for {
      header <- c.downField("header").as[Header]
      interlinks <- c.downField("interlinks").as[Seq[String]]
    } yield PoPowHeader(header, interlinks.map(s => ModifierId @@ s))
  }

}

object PoPowHeaderSerializer extends ScorexSerializer[PoPowHeader] {
  import org.ergoplatform.wallet.Constants.ModifierIdLength

  override def serialize(obj: PoPowHeader, w: Writer): Unit = {
    val headerBytes = obj.header.bytes
    w.putUInt(headerBytes.length)
    w.putBytes(headerBytes)
    w.putUInt(obj.interlinks.size)
    obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
  }

  override def parse(r: Reader): PoPowHeader = {
    val headerSize = r.getUInt().toIntExact
    val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
    val linksQty = r.getUInt().toIntExact
    val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(ModifierIdLength)))
    PoPowHeader(header, interlinks)
  }

}
