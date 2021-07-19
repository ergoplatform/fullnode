package org.ergoplatform.network

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{Message, MessageSerializer, ModifiersData, ModifiersSpec}
import scorex.util.encode.Base16

/**
  * Modifiers message used to send transactions or block parts to a remote peer
  */
class ModifiersSpecification extends ErgoPropertyTest with DecodingUtils {

  property("modifiers message reference parser") {
    val hBytes = Base16.decode("4201ad7fffba807080ce7f808001b4771880bdffbd7fff857f8db17fdc89b0015bff0189486d80934a84015e03c064c6d100c17fae55bf01f380007fbb00ff01d9000000013f0680d10015ff11c4b7ffff01ffb2d27f0001050064010080ffb93e7fff4cc15a2bffdd770480007f8080ff8026802f7f51007f004b01ff0080002380a581e8c0ca81dfca62007fa7ff01860019f37f3c4491067cd1001ee07e570fe8689b80ff11017aff7408703927a2bbc4fa0200000000032bdb4c610ddf0078b4f31f5a36bff7b003704dfc93b201fedd8fe331d9ff1102008d7fff010d80ff").get
    val header = HeaderSerializer.parseBytes(hBytes)

    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte) // mainnet magic
    val mSpec = new ModifiersSpec(maxMessageSize = 10000)

    val mData = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))

    val mMessage = Message(mSpec, Right(mData), None)

    val ms = new MessageSerializer(Seq(mSpec), magic)

    val bs = ms.serialize(mMessage).toArray
    val bsString = Base16.encode(bs)

    // test vector for external implementations
    bsString shouldBe "0100020421000001058fdd57ca65010d537f94ae67e5026fd4acee1cdd1e7281a73e94a7681864438c629e9683b3a5e1014201ad7fffba807080ce7f808001b4771880bdffbd7fff857f8db17fdc89b0015bff0189486d80934a84015e03c064c6d100c17fae55bf01f380007fbb00ff01d9000000013f0680d10015ff11c4b7ffff01ffb2d27f0001050064010080ffb93e7fff4cc15a2bffdd770480007f8080ff8026802f7f51007f004b01ff0080002380a581e8c0ca81dfca62007fa7ff01860019f37f3c4491067cd1001ee07e570fe8689b80ff11017aff7408703927a2bbc4fa0200000000032bdb4c610ddf0078b4f31f5a36bff7b003704dfc93b201fedd8fe331d9ff1102008d7fff010d80ff"

    val bb = ByteBuffer.wrap(bs)

    // simple reference parser below

    // read network magic (network id) bytes (4 bytes)
    val magicRead = getBytes(bb, 4)
    magicRead.toIndexedSeq shouldBe magic.toIndexedSeq

    // read message type id
    val messageCode = getByte(bb)
    messageCode shouldBe mSpec.messageCode  // 33 (in dec)

    // read message length (4 bytes)
    val messageLength = Ints.fromByteArray(getBytes(bb,4))

    messageLength shouldBe 261

    val checkSum = getBytes(bb, 4)

    // read modifier type id (1 byte)
    val modifierTypeId = getByte(bb) // should read one byte only

    modifierTypeId shouldBe 101.toByte // type id corresponding to block header

    // read number of modifiers (headers)
    val headersCount = getULong(bb).toInt // could read up to 4 bytes max

    headersCount shouldBe 1

    // read modifier (header) id
    val headerIdParsed = getBytes(bb, 32)

    Base16.encode(headerIdParsed) shouldBe header.id

    // read read modifier (header) bytes length
    val headerLength = getULong(bb).toInt // could read up to 4 bytes max

    // read read modifier (header) bytes
    val headerBytes = getBytes(bb, headerLength)

    headerBytes.toIndexedSeq shouldBe header.bytes.toIndexedSeq
  }

}
