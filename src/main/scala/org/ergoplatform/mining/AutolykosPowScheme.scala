package org.ergoplatform.mining

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer, HeaderWithoutPow}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import scorex.core.block.Block
import scorex.core.block.Block.{Timestamp, Version}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Try

/**
  * Autolykos PoW puzzle scheme reference implementation.
  *
  * Based on k-sum problem, so general idea is to find k numbers in a table of size N, such that
  * sum of numbers (or a hash of the sum) is less than target value.
  *
  * See https://docs.ergoplatform.com/ErgoPow.pdf for details
  *
  * CPU Mining process is implemented in inefficient way and should not be used in real environment.
  *
  * @see papers/yellow/pow/ErgoPow.tex for full description
  * @param k - number of elements in one solution
  * @param n - initially, N = 2^^n
  */
class AutolykosPowScheme(val k: Int, val n: Int) extends ScorexLogging {

  require(k <= 32, "k > 32 is not allowed due to genIndexes function")
  require(n < 31, "n >= 31 is not allowed")

  //Consensus-critical code below

  /**
    * Number of elements in a table to find k-sum problem solution on top of
    */
  val NBase: Int = Math.pow(2, n).toInt

  /**
    * Initial height since which table (`N` value) starting to increase by 5% per `IncreasePeriodForN` blocks
    */
  val IncreaseStart: Height = 600 * 1024

  /**
    * Table size (`N`) increased every 50 * 1024 blocks
    */
  val IncreasePeriodForN: Height = 50 * 1024

  /**
    * On this height, the table (`N` value) will stop to grow
    */
  val NIncreasementHeightMax: Height = 4198400

  /**
    * Calculates table size (N value) for a given height (moment of time)
    *
    * @see papers/yellow/pow/ErgoPow.tex for full description and test vectors
    * @param version - protocol version
    * @param headerHeight - height of a header to mine
    * @return - N value
    */
  def calcN(version: Version, headerHeight: Height): Int = {
    if (version == Header.InitialVersion) {
      NBase
    } else {
      val height = Math.min(NIncreasementHeightMax, headerHeight)
      if (height < IncreaseStart) {
        NBase
      } else {
        val itersNumber = (height - IncreaseStart) / IncreasePeriodForN + 1
        (1 to itersNumber).foldLeft(NBase) { case (step, _) =>
          step / 100 * 105
        }
      }
    }
  }

  def calcN(header: HeaderWithoutPow): Int = calcN(header.version, header.height)

  /**
    * Constant data to be added to hash function to increase its calculation time
    */
  val M: Array[Byte] = (0 until 1024).toArray.flatMap(i => Longs.toByteArray(i))

  /**
    * Checks that `header` contains correct solution of the Autolykos PoW puzzle.
    */
  def validate(header: Header): Try[Unit] = Try {
    val b = getB(header.nBits)
    if (header.version == 1) {
      // for version 1, we check equality of left and right sides of the equation
      require(checkPoWForVersion1(header, b), "Incorrect points")
    } else {
      // for version 2, we're calculating hit and compare it with target
      val hit = hitForVersion2(header)
      require(hit < b, "h(f) < b condition not met")
    }
  }

  /**
    * Check PoW for Autolykos v1 header
    *
    * @param header - header to check PoW for
    * @param b - PoW target
    * @return whether PoW is valid or not
    */
  def checkPoWForVersion1(header: Header, b: BigInt): Boolean = {
    val version = 1: Byte

    val msg = msgByHeader(header)
    val s = header.powSolution

    val nonce = s.n

    val N = calcN(header)

    require(s.d < b, s"Incorrect d = ${s.d} for b = $b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")

    val pkBytes = groupElemToBytes(s.pk)
    val wBytes = groupElemToBytes(s.w)

    val seed = Bytes.concat(msg, nonce) // Autolykos v1, Alg. 2, line4: m || nonce
    val indexes = genIndexes(seed, N)

    //height is not used in v1
    val f = indexes.map(idx => genElement(version, msg, pkBytes, wBytes, Ints.toByteArray(idx), null)).sum.mod(q)
    val left = s.w.multiply(f.bigInteger)
    val right = group.generator.multiply(s.d.bigInteger).add(s.pk)
    left == right
  }


  /**
    * Get hit for Autolykos v2 header (to test it then against PoW target)
    *
    * @param header - header to check PoW for
    * @return PoW hit
    */
  def hitForVersion2(header: Header): BigInt = {
    val version = 2: Byte

    val msg = msgByHeader(header)
    val nonce = header.powSolution.n

    val h = Ints.toByteArray(header.height)  // used in AL v.2 only

    val N = calcN(header)

    val prei8 = BigIntegers.fromUnsignedByteArray(hash(Bytes.concat(msg, nonce)).takeRight(8))
    val i = BigIntegers.asUnsignedByteArray(4, prei8.mod(BigInt(N).underlying()))
    val f = Blake2b256(Bytes.concat(i, h, M)).drop(1) // .drop(1) is the same as takeRight(31)
    val seed = Bytes.concat(f, msg, nonce) // Autolykos v1, Alg. 2, line4:

    val indexes = genIndexes(seed, N)
    //pk and w not used in v2
    val elems = indexes.map(idx => genElement(version, msg, null, null, Ints.toByteArray(idx), h))
    val f2 = elems.sum

    // sum as byte array is always about 32 bytes
    val array: Array[Byte] = BigIntegers.asUnsignedByteArray(32, f2.underlying())
    toBigInt(hash(array))
  }

  /**
    * Pow puzzle is solved if hit < target. This function calculates the hit.
    * The hit is also used as "real target" in NiPoPoWs.
    */
  def powHit(header: Header): BigInt = {
    if (header.version == 1) {
      header.powSolution.d
    } else {
      hitForVersion2(header)
    }
  }

  /**
    * Real difficulty of `header`.
    * May occasionally exceeds required difficulty due to random nature of PoW puzzle.
    * Used in NiPoPoW.
    */
  def realDifficulty(header: Header): BigInt = {
      q / powHit(header)
  }

  /**
    * Header digest ("message" for default GPU miners) a miner is working on
    */
  def msgByHeader(h: HeaderWithoutPow): Array[Byte] = Blake2b256(HeaderSerializer.bytesWithoutPow(h))

  /**
    * Get target `b` from encoded difficulty `nBits`
    */
  private[mining] def getB(nBits: Long): BigInt = {
    q / RequiredDifficulty.decodeCompactBits(nBits)
  }

  /**
    * Hash function that takes `m` and `nonceBytes` and returns a list of size `k` with numbers in
    * [0,`N`)
    */
  private def genIndexes(seed: Array[Byte], N: Int): Seq[Int] = {
    val hash = Blake2b256(seed)
    val extendedHash = Bytes.concat(hash, hash.take(3))
    (0 until k).map { i =>
      BigInt(1, extendedHash.slice(i, i + 4)).mod(N).toInt
    }
  }.ensuring(_.length == k)

  /**
    * Generate element of Autolykos equation.
    */
  private def genElement(version: Block.Version,
                         m: Array[Byte],
                         pk: Array[Byte],  // not used in v2
                         w: Array[Byte],   // not used in v2
                         indexBytes: Array[Byte],
                         heightBytes: => Array[Byte] // not used in v1
                        ): BigInt = {
    if (version == 1) {
      // Autolykos v. 1: H(j|M|pk|m|w) (line 5 from the Algo 2 of the spec)
      hashModQ(Bytes.concat(indexBytes, M, pk, m, w))
    } else {
      // Autolykos v. 2: H(j|h|M) (line 5 from the Algo 2 of the spec)
      toBigInt(hash(Bytes.concat(indexBytes, heightBytes, M)).drop(1))
    }
  }

  //Proving-related code which is not critical for consensus below

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that header with the specified fields will contain
    * correct solution of the Autolykos PoW puzzle.
    */
  def prove(parentOpt: Option[Header],
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

    val h = HeaderWithoutPow(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, votes)
    val msg = msgByHeader(h)
    val b = getB(nBits)
    val x = randomSecret()
    val hbs = Ints.toByteArray(h.height)
    val N = calcN(h)
    checkNonces(version, hbs, msg, sk, x, b, N, minNonce, maxNonce).map(solution => h.toHeader(solution))
  }

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that full block with the specified fields will contain
    * correct solution of the Autolykos PoW puzzle.
    */
  def proveBlock(parentOpt: Option[Header],
                 version: Block.Version,
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[ErgoTransaction],
                 timestamp: Timestamp,
                 extensionCandidate: ExtensionCandidate,
                 votes: Array[Byte],
                 sk: PrivateKey,
                 minNonce: Long = Long.MinValue,
                 maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.transactionsRoot(transactions, version)
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionCandidate.digest, votes, sk, minNonce, maxNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      val blockTransactions = BlockTransactions(h.id, version, transactions)
      val extension = extensionCandidate.toExtension(h.id)
      new ErgoFullBlock(h, blockTransactions, extension, Some(adProofs))
    }
  }

  /**
    * Find a nonce from `minNonce` to `maxNonce`, such that full block created from block candidate `candidateBlock`
    * will contain correct solution of the Autolykos PoW puzzle.
    */
  def proveCandidate(candidateBlock: CandidateBlock,
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {
    proveBlock(candidateBlock.parentOpt,
      candidateBlock.version,
      candidateBlock.nBits,
      candidateBlock.stateRoot,
      candidateBlock.adProofBytes,
      candidateBlock.transactions,
      candidateBlock.timestamp,
      candidateBlock.extension,
      candidateBlock.votes,
      sk,
      minNonce,
      maxNonce
    )
  }

  /**
    * Check nonces from `startNonce` to `endNonce` for message `m`, secrets `sk` and `x`, difficulty `b`.
    * Return AutolykosSolution if there is any valid nonce in this interval.
    */
  private[mining] def checkNonces(version: Block.Version,
                                  h: Array[Byte],
                                  m: Array[Byte],
                                  sk: BigInt,
                                  x: BigInt,
                                  b: BigInt,
                                  N: Int,
                                  startNonce: Long,
                                  endNonce: Long): Option[AutolykosSolution] = {
    log.debug(s"Going to check nonces from $startNonce to $endNonce")
    val p1 = groupElemToBytes(genPk(sk))
    val p2 = groupElemToBytes(genPk(x))

    @tailrec
    def loop(i: Long): Option[AutolykosSolution] = if (i == endNonce) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log.debug(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val seed = if(version == 1) {
        Bytes.concat(m, nonce)
      } else {
        val i = BigIntegers.asUnsignedByteArray(4, BigIntegers.fromUnsignedByteArray(hash(Bytes.concat(m, nonce)).takeRight(8)).mod(BigInt(N).underlying()))
        val f = Blake2b256(Bytes.concat(i, h, M)).drop(1)
        Bytes.concat(f, m, nonce)
      }
      val d = if(version == 1) {
        (x * genIndexes(seed, N).map(i => genElement(version, m, p1, p2, Ints.toByteArray(i), h)).sum - sk).mod(q)
      } else {
        val indexes = genIndexes(seed, N)
        toBigInt(hash(indexes.map(i => genElement(version, m, p1, p2, Ints.toByteArray(i), h)).sum.toByteArray))
      }
      if (d <= b) {
        log.debug(s"Solution found at $i")
        Some(AutolykosSolution(genPk(sk), genPk(x), nonce, d))
      } else {
        loop(i + 1)
      }
    }

    loop(startNonce)
  }

  /**
    * Assembles candidate block for external miners.
    */
  def deriveExternalCandidate(candidate: CandidateBlock, pk: ProveDlog): WorkMessage =
    deriveExternalCandidate(candidate, pk, Seq.empty)

  /**
    * Assembles candidate block for external miner with certain transactions included into the block
    *
    * @param blockCandidate      - block candidate (contained the transactions)
    * @param pk             - miner pubkey
    * @param mandatoryTxIds - ids of the transactions to include
    * @return - block candidate for external miner
    */
  def deriveExternalCandidate(blockCandidate: CandidateBlock,
                              pk: ProveDlog,
                              mandatoryTxIds: Seq[ModifierId]): WorkMessage = {
    val headerCandidate = ErgoMiner.deriveUnprovenHeader(blockCandidate)
    val msg = msgByHeader(headerCandidate)
    val b = getB(blockCandidate.nBits)
    val hOpt = if (blockCandidate.version == 1) {
      None
    } else {
      Some(headerCandidate.height)
    }

    val proofs = if (mandatoryTxIds.nonEmpty) {
      // constructs fake block transactions section (BlockTransactions instance) to get proofs from it
      val fakeHeaderId = scorex.util.bytesToId(Array.fill(org.ergoplatform.wallet.Constants.ModifierIdLength)(0: Byte))
      val bt = BlockTransactions(fakeHeaderId, blockCandidate.version, blockCandidate.transactions)
      val ps = mandatoryTxIds.flatMap { txId => bt.proofFor(txId).map(mp => TransactionMembershipProof(txId, mp)) }
      Some(ProofOfUpcomingTransactions(headerCandidate, ps))
    } else {
      None
    }
    WorkMessage(msg, b, hOpt, pk, proofs)
  }

}

object AutolykosPowScheme {

  /**
    * Calculate header fields based on its parent, namely, header's parent id and height
    */
  def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Int) = {

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(ErgoHistory.GenesisHeight)

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, height)
  }

}
