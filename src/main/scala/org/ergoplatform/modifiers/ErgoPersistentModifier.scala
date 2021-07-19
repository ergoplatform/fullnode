package org.ergoplatform.modifiers

import io.circe.Encoder
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import scorex.core.PersistentNodeViewModifier

trait ErgoPersistentModifier extends PersistentNodeViewModifier with ErgoNodeViewModifier

object ErgoPersistentModifier {

  implicit val jsonEncoder: Encoder[ErgoPersistentModifier] = {
    case h: Header => Header.jsonEncoder(h)
    case bt: BlockTransactions => BlockTransactions.jsonEncoder(bt)
    case adp: ADProofs => ADProofs.jsonEncoder(adp)
    case ext: Extension => Extension.jsonEncoder(ext)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }

}
