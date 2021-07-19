package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionSerializer}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

object HistoryModifierSerializer extends ScorexSerializer[ErgoPersistentModifier] {

  override def serialize(obj: ErgoPersistentModifier, w: Writer): Unit = {
    obj match {
      case m: Header =>
        w.put(Header.modifierTypeId)
        HeaderSerializer.serialize(m, w)
      case m: ADProofs =>
        w.put(ADProofs.modifierTypeId)
        ADProofsSerializer.serialize(m, w)
      case m: BlockTransactions =>
        w.put(BlockTransactions.modifierTypeId)
        BlockTransactionsSerializer.serialize(m, w)
      case m: Extension =>
        w.put(Extension.modifierTypeId)
        ExtensionSerializer.serialize(m, w)
      case m =>
        throw new Error(s"Serialization for unknown modifier: $m")
    }
  }

  override def parse(r: Reader): ErgoPersistentModifier = {
    r.getByte() match {
      case Header.`modifierTypeId` =>
        HeaderSerializer.parse(r)
      case ADProofs.`modifierTypeId` =>
        ADProofsSerializer.parse(r)
      case BlockTransactions.`modifierTypeId` =>
        BlockTransactionsSerializer.parse(r)
      case Extension.`modifierTypeId` =>
        ExtensionSerializer.parse(r)
      case m =>
        throw new Error(s"Deserialization for unknown type byte: $m")
    }
  }
}
