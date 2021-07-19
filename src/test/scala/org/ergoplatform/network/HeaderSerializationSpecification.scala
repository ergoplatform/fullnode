package org.ergoplatform.network

import java.nio.ByteBuffer

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosSolution, groupElemFromBytes}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.interpreter.CryptoConstants.EcPointType

class HeaderSerializationSpecification extends ErgoPropertyTest with DecodingUtils {

  private def base16ToEcPoint(pointEncoded: String): EcPointType = {
    val bytes = Algos.decode(pointEncoded).get
    groupElemFromBytes(bytes)
  }

  // This test contains simple parser for header version 1
  // In Ergo mainnet, used for blocks till 417,791
  property("Header simple parsing - block version 1") {
    // real header from mainnet, at 414,474, https://explorer.ergoplatform.com/en/blocks/8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f
    val version = 1 : Byte
    val height = 414474
    val parentId = ModifierId @@ "8bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e"
    val adProofsRoot = Digest32 @@ Base16.decode("4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0").get
    val stateRoot = ADDigest @@ Base16.decode("6c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512").get
    val transactionsRoot = Digest32 @@ Base16.decode("722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951").get
    val timestamp = 1611874199636L
    val nBits = 118039443L
    val extensionRoot = Digest32 @@ Base16.decode("a1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4").get

    val pk = base16ToEcPoint("02bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3")
    val w = base16ToEcPoint("026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14")
    val n = Base16.decode("0008a1d103880117").get
    val d = BigInt("35863003992655055679291741607273543535646500642591973829915050")
    val powSolution = AutolykosSolution(pk, w, n, d)
    val votes = Array[Byte](4, 3, 0)

    val h = Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
                    height, extensionRoot, powSolution, votes)

    h.id shouldBe "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f"

    h.id shouldBe Base16.encode(Blake2b256(h.bytes)) // header id is blake2b256 of its bytes


    //test vector
    Base16.encode(h.bytes) shouldBe
      "018bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb459516c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512d4d0c1d9f42ea1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4070923938aa61904030002bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb140008a1d1038801171a16514e604d76c516eec4124f4066e6e326b9e8d2fc5165b631aa"

    h.bytes.length shouldBe 279

    val bb = ByteBuffer.wrap(h.bytes)

    // read block version, 1 byte
    val versionParsed = getByte(bb)
    versionParsed shouldBe version

    // read parent header id, 32 bytes
    val parentIdParsed = getBytes(bb, 32)
    Base16.encode(parentIdParsed) shouldBe parentId

    // read authenticating hash of state transformation correctness proofs, 32 bytes
    val adProofsRootParsed = getBytes(bb, 32)
    adProofsRootParsed.toIndexedSeq shouldBe adProofsRoot.toIndexedSeq

    // read transactions Merkle tree root, 32 bytes
    val transactionsRootParsed = getBytes(bb, 32)
    transactionsRootParsed.toIndexedSeq shouldBe transactionsRoot.toIndexedSeq

    // read UTXO state AVL+ tree root + height, 33 bytes
    val stateRootParsed = getBytes(bb, 33)
    stateRootParsed.toIndexedSeq shouldBe stateRoot.toIndexedSeq

    val timestampParsed = getULong(bb) // timestamp, up to 8 bytes
    timestampParsed shouldBe timestamp

    // read Merkle tree root of block extension data, 32 bytes
    val extensionRootParsed = getBytes(bb, 32)
    extensionRootParsed.toIndexedSeq shouldBe extensionRoot.toIndexedSeq

    // read difficulty encoded in Bitcoin nBits format, https://bitco.in/en/developer-reference#target-nbits
    val nbits = getBytes(bb, 4) // 4 bytes
    val difficulty = RequiredDifficulty.decodeCompactBits(RequiredDifficulty.readUint32BE(nbits))
    difficulty shouldBe h.requiredDifficulty

    val heightParsed = getULong(bb) // up to 4 bytes
    heightParsed shouldBe height

    // read miner votes for protocol parameters update
    val votesParsed = getBytes(bb, 3)
    votesParsed.toIndexedSeq shouldBe votes.toIndexedSeq

    // read PoW solution
    // read public key, 33 bytes (EC point)
    val pkParsed = getBytes(bb, 33)
    groupElemFromBytes(pkParsed) shouldBe pk

    // read one-time secret w, 33 bytes (EC point)
    val wParsed = getBytes(bb, 33)
    groupElemFromBytes(wParsed) shouldBe w

    // read nonce (8 bytes)
    val nonceParsed = getBytes(bb, 8)
    nonceParsed.toIndexedSeq shouldBe n.toIndexedSeq

    // read length of d, 1 byte
    val dLength = getUByte(bb)
    // read d bigint bytes
    val dBytes = getBytes(bb, dLength)
    val dParsed = BigInt(1, dBytes)
    dParsed shouldBe d
  }


  // This test contains simple parser for header version 2
  // In Ergo mainnet, used for blocks since 417,792 (inclusive)
  property("Header simple parsing - block version 2") {
    // real header from mainnet, at 418,838, https://explorer.ergoplatform.com/en/blocks/f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39
    val version = 2 : Byte
    val height = 418138
    val parentId = ModifierId @@ "7fbc70ec5913706ddef67bbcdb7700ea5f15dc709012491269c9c7eb545d720c"
    val adProofsRoot = Digest32 @@ Base16.decode("a80bbd4d69b4f017da6dd9250448ef1cde492121fc350727e755c7b7ae2988ad").get
    val stateRoot = ADDigest @@ Base16.decode("995c0efe63744c5227e6ae213a2061c60f8db845d47707a6bff53f9ff1936a9e13").get
    val transactionsRoot = Digest32 @@ Base16.decode("141bf3de015c44995858a435e4d6c50c51622d077760de32977ba5412aaaae03").get
    val timestamp = 1612465607426L
    val nBits = 107976917L
    val extensionRoot = Digest32 @@ Base16.decode("b1457df896bba9dc962f8e42187e1ac580842f1282c8c7fb9cf9f4cd520d1c07").get

    val pk = base16ToEcPoint("0315345f1fca9445eee5df74759d4c495094bcfc82a2831b26fca6efa599b509de")
    val w = AutolykosSolution.wForV2
    val n = Base16.decode("1b95db2168f95fda").get
    val d = AutolykosSolution.dForV2
    val powSolution = AutolykosSolution(pk, w, n, d)
    val votes = Array[Byte](0, 0, 0)

    val h = Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
      height, extensionRoot, powSolution, votes)

    h.id shouldBe "f46c89e44f13a92d8409341490f97f05c85785fa8d2d2164332cc066eda95c39"

    h.id shouldBe Base16.encode(Blake2b256(h.bytes)) // header id is blake2b256 of its bytes

    val bb = ByteBuffer.wrap(h.bytes)

    // read block version
    val versionParsed = getByte(bb)
    versionParsed shouldBe version

    // read parent id, 32 bytes
    val parentIdParsed = getBytes(bb, 32)
    Base16.encode(parentIdParsed) shouldBe parentId

    // read authenticating hash of state transformation correctness proofs, 32 bytes
    val adProofsRootParsed = getBytes(bb, 32)
    adProofsRootParsed.toIndexedSeq shouldBe adProofsRoot.toIndexedSeq

    // read transactions Merkle tree root, 32 bytes
    val transactionsRootParsed = getBytes(bb, 32)
    transactionsRootParsed.toIndexedSeq shouldBe transactionsRoot.toIndexedSeq

    // read UTXO state AVL+ tree root + height, 33 bytes
    val stateRootParsed = getBytes(bb, 33)
    stateRootParsed.toIndexedSeq shouldBe stateRoot.toIndexedSeq

    val timestampParsed = getULong(bb) // timestamp, up to 8 bytes
    timestampParsed shouldBe timestamp

    // read Merkle tree root of block extension data, 32 bytes
    val extensionRootParsed = getBytes(bb, 32)
    extensionRootParsed.toIndexedSeq shouldBe extensionRoot.toIndexedSeq

    // read difficulty encoded in Bitcoin nBits format, https://bitco.in/en/developer-reference#target-nbits
    val nbits = getBytes(bb, 4) // 4 bytes
    val difficulty = RequiredDifficulty.decodeCompactBits(RequiredDifficulty.readUint32BE(nbits))
    difficulty shouldBe h.requiredDifficulty

    val heightParsed = getULong(bb) // up to 4 bytes
    heightParsed shouldBe height

    // read miner votes for protocol parameters update
    val votesParsed = getBytes(bb, 3)
    votesParsed.toIndexedSeq shouldBe votes.toIndexedSeq

    // Block version V2 specific field, contains length of additional data
    val additionalFieldsLength = getUByte(bb)
    additionalFieldsLength shouldBe 0

    // read PoW solution, no "w" and "d" in block version 2

    val pkParsed = getBytes(bb, 33)
    groupElemFromBytes(pkParsed) shouldBe pk

    val nonceParsed = getBytes(bb, 8)
    nonceParsed.toIndexedSeq shouldBe n.toIndexedSeq

    bb.remaining() shouldBe 0
  }

}
