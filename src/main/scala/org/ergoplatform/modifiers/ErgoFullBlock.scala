package org.ergoplatform.modifiers

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.serialization.Serializer
import scorex.core.{ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}
import scorex.util.ModifierId

case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         extension: Extension,
                         adProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  override type M = ErgoFullBlock

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override def serializedId: Array[Byte] = header.serializedId

  override lazy val id: ModifierId = header.id

  override def parentId: ModifierId = header.parentId

  lazy val mandatoryBlockSections: Seq[BlockSection] = Seq(blockTransactions, extension)

  lazy val blockSections: Seq[BlockSection] = adProofs.toSeq ++ mandatoryBlockSections

  lazy val toSeq: Seq[ErgoPersistentModifier] = header +: blockSections

  override lazy val transactions: Seq[ErgoTransaction] = blockTransactions.txs

  override val sizeOpt: Option[Int] = None

  override lazy val size: Int = header.size + blockTransactions.size + adProofs.map(_.size).getOrElse(0)

  override lazy val serializer: Serializer[ErgoFullBlock] =
    throw new Error("Serialization for ErgoFullBlock is not (and will not be) implemented")
}

object ErgoFullBlock extends ApiCodecs {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = { b: ErgoFullBlock =>
    Json.obj(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "extension" -> b.extension.asJson,
      "adProofs" -> b.adProofs.asJson,
      "size" -> b.size.asJson
    )
  }

  implicit val jsonDecoder: Decoder[ErgoFullBlock] = { c: HCursor =>
    for {
      header <- c.downField("header").as[Header]
      transactions <- c.downField("blockTransactions").as[BlockTransactions]
      extension <- c.downField("extension").as[Extension]
      adProofs <- c.downField("adProofs").as[Option[ADProofs]]
    } yield ErgoFullBlock(header, transactions, extension, adProofs)
  }

  val blockSizeEncoder: Encoder[ErgoFullBlock] = { b: ErgoFullBlock =>
    Json.obj(
      "id" -> b.header.id.asJson,
      "size" -> b.size.asJson
    )
  }
}
