package org.ergoplatform.modifiers.history.header

import org.ergoplatform.mining.AutolykosSolutionSerializer
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import scorex.core.idToBytes
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, VLQByteBufferWriter, Writer}
import scorex.util.{ByteArrayBuilder, bytesToId}
import scorex.util.Extensions._

object HeaderSerializer extends ScorexSerializer[Header] {

  override def serialize(h: Header, w: Writer): Unit = {
    serializeWithoutPow(h, w)
    AutolykosSolutionSerializer.serialize(h.version, h.powSolution, w)
  }

  def serializeWithoutPow(h: HeaderWithoutPow, w: Writer): Unit = {
    w.put(h.version)
    w.putBytes(idToBytes(h.parentId))
    w.putBytes(h.ADProofsRoot)
    w.putBytes(h.transactionsRoot)
    w.putBytes(h.stateRoot)
    w.putULong(h.timestamp)
    w.putBytes(h.extensionRoot)
    RequiredDifficulty.serialize(h.nBits, w)
    w.putUInt(h.height)
    w.putBytes(h.votes)

    // For block version >= 2, this new byte encodes length of possible new fields.
    // Set to 0 for now, so no new fields.
    if (h.version > Header.InitialVersion) {
      w.putUByte(0: Byte)
    }
  }

  def bytesWithoutPow(header: HeaderWithoutPow): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    serializeWithoutPow(header, w)
    w.result().toBytes
  }

  def parseWithoutPow(r: Reader): HeaderWithoutPow = {
    val version = r.getByte()
    val parentId = bytesToId(r.getBytes(32))
    val ADProofsRoot = Digest32 @@ r.getBytes(32)
    val transactionsRoot = Digest32 @@ r.getBytes(32)
    val stateRoot = ADDigest @@ r.getBytes(33)
    val timestamp = r.getULong()
    val extensionHash = Digest32 @@ r.getBytes(32)
    val nBits = RequiredDifficulty.parse(r)
    val height = r.getUInt().toIntExact
    val votes = r.getBytes(3)

    // For block version >= 2, a new byte encodes length of possible new fields.
    // If this byte > 0, we read new fields but do nothing, as semantics of the fields is not known.
    if (version > Header.InitialVersion) {
      val newFieldsSize = r.getUByte()
      if (newFieldsSize > 0) {
        r.getBytes(newFieldsSize)
      }
    }

    HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, votes)
  }

  override def parse(r: Reader): Header = {
    val headerWithoutPow = parseWithoutPow(r)
    val powSolution = AutolykosSolutionSerializer.parse(r, headerWithoutPow.version)
    headerWithoutPow.toHeader(powSolution, Some(r.consumed))
  }

}
