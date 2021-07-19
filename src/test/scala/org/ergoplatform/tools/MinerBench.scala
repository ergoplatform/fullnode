package org.ergoplatform.tools

import com.google.common.primitives.Bytes
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.utils.ErgoTestHelpers
import scorex.crypto.hash.{Blake2b256, Blake2b512, CryptographicHash, Digest}

import scala.annotation.tailrec

object MinerBench extends App with ErgoTestHelpers {

  validationBench()
  //  numericHashBench()

  def numericHashBench(): Unit = {
    val Steps = 100000000

    def numericHash(input0: Array[Byte], hf: CryptographicHash[_ <: Digest], validRange: BigInt): Unit = {
      @tailrec
      def hash(input: Array[Byte]): BigInt = {
        val hashed = hf.apply(input)
        val bi = BigInt(BigIntegers.fromUnsignedByteArray(hashed))
        if (bi < validRange) {
          bi.mod(q)
        } else {
          log.debug(s"Calculate one more hash for ${encoder.encode(input)} and p=$q")
          hash(hashed)
        }
      }
    }

    val data = Bytes.concat(Array.fill(16486)(1.toByte))
    // test Blake2b256
    val validRange256: BigInt = (BigInt(2).pow(Blake2b256.DigestSize * 8) / q) * q

    // prepare JVM
    (0 until Steps).foreach(_ => numericHash(data, Blake2b256, validRange256))

    val st = System.currentTimeMillis()
    (0 until Steps).foreach(_ => numericHash(data, Blake2b256, validRange256))
    val st2 = System.currentTimeMillis()

    val validRange512: BigInt = (BigInt(2).pow(Blake2b512.DigestSize * 8) / q) * q

    // prepare JVM
    (0 until Steps).foreach(_ => numericHash(data, Blake2b512, validRange512))

    val st3 = System.currentTimeMillis()
    (0 until Steps).foreach(_ => numericHash(data, Blake2b512, validRange512))
    val st4 = System.currentTimeMillis()

    println(s"Calculation time of $Steps numberic hashes over ${data.length} bytes")
    println(s"Blake2b256: ${st2 - st} ms")
    println(s"Blake2b512: ${st4 - st3} ms")

  }

  def validationBench() {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    val sk = randomSecret()
    val difficulty = 1000
    val fb = invalidErgoFullBlockGen.sample.get
    val inHeader = fb.header
    val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
    val h = inHeader.copy(nBits = nBits)

    val candidate = new CandidateBlock(None, Header.InitialVersion, nBits: Long, h.stateRoot,
      fb.adProofs.get.proofBytes,
      fb.blockTransactions.txs,
      System.currentTimeMillis(),
      ExtensionCandidate(Seq.empty),
      Array())
    val newHeader = pow.proveCandidate(candidate, sk).get.header

    val Steps = 10000

    (0 until Steps / 10) foreach (_ => pow.validate(newHeader))

    val st = System.currentTimeMillis()

    (0 until Steps) foreach (_ => pow.validate(newHeader))

    println(s"M = ${pow.M.length / 1024} Kb:${(System.currentTimeMillis() - st).toDouble / Steps} ms")
  }

}
