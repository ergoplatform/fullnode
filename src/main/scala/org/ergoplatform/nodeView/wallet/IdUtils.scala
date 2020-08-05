package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util._

object IdUtils {


  type EncodedBoxId = ModifierId

  type EncodedTokenId = ModifierId

  def encodedBoxId(id: BoxId): EncodedBoxId = bytesToId(id)

  def decodedBoxId(id: EncodedBoxId): BoxId = ADKey @@ idToBytes(id)

  def encodedTokenId(id: TokenId): EncodedTokenId = bytesToId(id)

  def decodedTokenId(id: EncodedTokenId): TokenId = Digest32 @@ idToBytes(id)

}
