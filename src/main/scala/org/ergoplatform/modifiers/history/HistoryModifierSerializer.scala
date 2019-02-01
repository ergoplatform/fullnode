package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.state.{UtxoSnapshotChunk, UtxoSnapshotChunkSerializer, UtxoSnapshotManifest,
  UtxoSnapshotManifestSerializer}
import scorex.core.serialization.Serializer

import scala.util.{Failure, Try}

object HistoryModifierSerializer extends Serializer[ErgoPersistentModifier] {

  override def toBytes(obj: ErgoPersistentModifier): Array[Byte] = obj match {
    case m: Header =>
      Header.modifierTypeId +: HeaderSerializer.toBytes(m)
    case m: ADProofs =>
      ADProofs.modifierTypeId +: ADProofSerializer.toBytes(m)
    case m: BlockTransactions =>
      BlockTransactions.modifierTypeId +: BlockTransactionsSerializer.toBytes(m)
    case m: Extension =>
      Extension.modifierTypeId +: ExtensionSerializer.toBytes(m)
    case m: UtxoSnapshotManifest =>
      UtxoSnapshotManifest.modifierTypeId +: UtxoSnapshotManifestSerializer.toBytes(m)
    case m: UtxoSnapshotChunk =>
      UtxoSnapshotChunk.modifierTypeId +: UtxoSnapshotChunkSerializer.toBytes(m)
    case m =>
      throw new Error(s"Serialization for unknown modifier: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoPersistentModifier] = Try(bytes.head).flatMap {
    case Header.`modifierTypeId` =>
      HeaderSerializer.parseBytes(bytes.tail)
    case ADProofs.`modifierTypeId` =>
      ADProofSerializer.parseBytes(bytes.tail)
    case BlockTransactions.`modifierTypeId` =>
      BlockTransactionsSerializer.parseBytes(bytes.tail)
    case Extension.`modifierTypeId` =>
      ExtensionSerializer.parseBytes(bytes.tail)
    case UtxoSnapshotManifest.`modifierTypeId` =>
      UtxoSnapshotManifestSerializer.parseBytes(bytes.tail)
    case UtxoSnapshotChunk.`modifierTypeId` =>
      UtxoSnapshotChunkSerializer.parseBytes(bytes.tail)
    case m =>
      Failure(new Error(s"Deserialization for unknown type byte: $m"))
  }

}
