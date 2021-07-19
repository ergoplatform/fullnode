package org.ergoplatform.network

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{InvData, Message, MessageSerializer, RequestModifierSpec}
import scorex.crypto.hash
import scorex.util.ModifierId
import scorex.util.encode.Base16

/**
  * RequestModifiers message is used to ask peers about transactions and block parts to download
  */
class RequestModifiersSpecification extends ErgoPropertyTest with DecodingUtils {

  property("requestModifiers reference parser") {

    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte) // mainnet magic
    val rmSpec = new RequestModifierSpec(maxInvObjects = 100)

    val headerId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)

    val headerIdEncoded = ModifierId @@ Base16.encode(headerId)

    val invData = InvData(Header.modifierTypeId, Seq(headerIdEncoded))

    val rmMessage = Message(rmSpec, Right(invData), None)

    val ms = new MessageSerializer(Seq(rmSpec), magic)

    val bs = ms.serialize(rmMessage).toArray
    val bsString = Base16.encode(bs)

    // test vector for external implementations
    bsString shouldBe "0100020416000000226abfdbf565010101010101010101010101010101010102020202020202020202020202020202"

    val bb = ByteBuffer.wrap(bs)

    // simple reference parser below

    // read network magic (network id) bytes (4 bytes)
    val magicRead = getBytes(bb, 4)
    magicRead.toIndexedSeq shouldBe magic.toIndexedSeq

    // read message type id
    val messageCode = getByte(bb)
    messageCode shouldBe rmSpec.messageCode  // 22 (in dec)

    // read message length (4 bytes)
    val messageLength = Ints.fromByteArray(getBytes(bb,4))

    messageLength shouldBe 34

    val checkSum = getBytes(bb, 4)

    // read modifier type id (1 byte)
    val modifierTypeId = getByte(bb) // should read one byte only

    modifierTypeId shouldBe 101.toByte // type id corresponding to block header

    // read number of modifiers (headers)
    val headersCount = getULong(bb).toInt // should read up to 4 bytes max

    headersCount shouldBe 1

    // read mofifier (header) ids
    val headerIdParsed = getBytes(bb, 32)

    headerIdParsed.toIndexedSeq shouldBe headerId.toIndexedSeq

    // validating checksum
    checkSum shouldBe hash.Blake2b256(Array(modifierTypeId, headersCount.toByte) ++ headerId).take(4)
  }

}
