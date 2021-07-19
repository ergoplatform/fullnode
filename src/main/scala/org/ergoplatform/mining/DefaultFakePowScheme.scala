package org.ergoplatform.mining

import org.ergoplatform.modifiers.history.header.Header
import scorex.core.block.Block
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.math.BigInt
import scala.util.{Random, Success, Try}

/**
  * Fake Pow Scheme for tests.
  * Fill solution with random values, as any block is valid during validation
  */
class DefaultFakePowScheme(k: Int, n: Int) extends AutolykosPowScheme(k, n) {
  override def validate(header: Header): Try[Unit] = Success(Unit)

  override def prove(parentOpt: Option[Header],
                     version: Block.Version,
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     extensionHash: Digest32,
                     votes: Array[Byte],
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[Header] = {
    val (parentId, height) = AutolykosPowScheme.derivedHeaderFields(parentOpt)
    val pk: EcPointType = genPk(sk)
    val w: EcPointType = genPk(Random.nextLong())
    val n: Array[Byte] = Array.fill(8)(0: Byte)
    val d: BigInt = q / (height + 10)
    val s = AutolykosSolution(pk, w, n, d)
    Some(Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, s, votes))
  }

  override def realDifficulty(header: Header): PrivateKey = header.requiredDifficulty

}
