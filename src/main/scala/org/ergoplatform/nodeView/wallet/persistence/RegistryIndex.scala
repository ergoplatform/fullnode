package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

final case class RegistryIndex(height: Int,
                               balance: Long,
                               assetBalances: Map[EncodedTokenId, Long],
                               uncertainBoxes: Seq[EncodedBoxId]) {

  override def equals(obj: Any): Boolean = obj match {
    case that: RegistryIndex =>
      val equalHeight = that.height == this.height
      val equalBalance = that.balance == this.balance
      val equalTokens = that.assetBalances.sameElements(this.assetBalances)
      val equalUncertain = that.uncertainBoxes.sameElements(this.uncertainBoxes)
      equalHeight && equalBalance && equalTokens && equalUncertain
    case _ =>
      false
  }

}

object RegistryIndex {

  def empty: RegistryIndex =
    RegistryIndex(ErgoHistory.EmptyHistoryHeight, 0, Map.empty, Seq.empty)

}

object RegistryIndexSerializer extends ScorexSerializer[RegistryIndex] {

  override def serialize(obj: RegistryIndex, w: Writer): Unit = {
    w.putInt(obj.height)
    w.putLong(obj.balance)
    w.putInt(obj.assetBalances.size)
    obj.assetBalances.foreach { case (id, amt) =>
      w.putBytes(decodedTokenId(id))
      w.putLong(amt)
    }
    w.putInt(obj.uncertainBoxes.size)
    obj.uncertainBoxes.foreach(x => w.putBytes(decodedBoxId(x)))
  }

  override def parse(r: Reader): RegistryIndex = {
    val height = r.getInt()
    val balance = r.getLong()
    val tokensQty = r.getInt()
    val tokenBalances = (0 until tokensQty).map { _ =>
      encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getLong()
    }
    val uncertainQty = r.getInt()
    val uncertainIds = (0 until uncertainQty).map { _ =>
      encodedBoxId(ADKey @@ r.getBytes(Constants.ModifierIdSize))
    }
    RegistryIndex(height, balance, tokenBalances.toMap, uncertainIds)
  }

}
