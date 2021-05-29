package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderChain}
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.core.consensus.History._
import scorex.crypto.hash.Digest32

import scala.util.Random

class NonVerifyADHistorySpecification extends HistoryTestHelpers {

  private def genHistory() =
    generateHistory(verifyTransactions = false, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 0, epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private lazy val popowHistory = ensureMinimalHeight(genHistory(), 100)

  ignore("Should apply UTXOSnapshotChunks") {
    forAll(randomUTXOSnapshotChunkGen) { snapshot: UTXOSnapshotChunk =>
      popowHistory.applicable(snapshot) shouldBe true
      val processInfo = popowHistory.append(snapshot).get._2
      processInfo.toApply shouldEqual Some(snapshot)
      popowHistory.applicable(snapshot) shouldBe false
    }
  }

  property("Should calculate difficulty correctly") {
    val epochLength = 3
    val useLastEpochs = 3

    val initDiff = BigInt(2)
    val initDiffBits = RequiredDifficulty.encodeCompactBits(initDiff)

    var history = generateHistory(
      verifyTransactions = false,
      StateType.Digest,
      PoPoWBootstrap = false,
      blocksToKeep = 0,
      epochLength = epochLength,
      useLastEpochs = useLastEpochs,
      initialDiffOpt = Some(initDiff)
    )
    val blocksBeforeRecalculate = epochLength + 1

    history = applyHeaderChain(history,
      genHeaderChain(blocksBeforeRecalculate, history, diffBitsOpt = Some(initDiffBits), useRealTs = true))

    val bestHeaderOpt = history.bestHeaderOpt

    history.requiredDifficultyAfter(bestHeaderOpt.get) shouldBe initDiff
  }

  property("lastHeaders() should return correct number of blocks") {
    forAll(smallInt) { m =>
      val lastHeaders = popowHistory.lastHeaders(m)
      if (m > 0) {
        lastHeaders.last shouldBe popowHistory.bestHeaderOpt.get
      }
      lastHeaders.length shouldBe m
    }
  }

  property("History.isInBestChain") {
    var history = genHistory()
    val common = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    val fork2 = genHeaderChain(BlocksInChain + 1, history, diffBitsOpt = None, useRealTs = false)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last
    fork1.headers.foreach(h => history.isInBestChain(h.id) shouldBe true)

    history = applyHeaderChain(history, fork2.tail)
    history.bestHeaderOpt.get shouldBe fork2.last
    fork2.headers.foreach(h => history.isInBestChain(h.id) shouldBe true)
    fork1.tail.headers.foreach(h => history.isInBestChain(h.id) shouldBe false)
  }

  property("Compare headers chain") {
    var history = genHistory()

    def getInfo(c: HeaderChain): ErgoSyncInfo = ErgoSyncInfo(c.headers.map(_.id))

    val common = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    val fork2 = genHeaderChain(BlocksInChain + 1, history, diffBitsOpt = None, useRealTs = false)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last

    history.compare(getInfo(fork2)) shouldBe Fork
    history.compare(getInfo(fork1)) shouldBe Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe Fork
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe Fork
    history.compare(getInfo(fork2.tail)) shouldBe Older
  }

  property("continuationIds() on forks") {
    var history1 = genHistory()
    var history2 = genHistory()
    val inChain = genHeaderChain(2, history1, diffBitsOpt = None, useRealTs = false)


    //put genesis
    history1 = applyHeaderChain(history1, inChain)
    history2 = applyHeaderChain(history2, inChain)
    val fork1 = genHeaderChain(BlocksInChain, history1, diffBitsOpt = None, useRealTs = false).tail
    val fork2 = genHeaderChain(BlocksInChain, history1, diffBitsOpt = None, useRealTs = false).tail

    //apply 2 different forks
    history1 = applyHeaderChain(history1, fork1)
    history2 = applyHeaderChain(history2, fork1.take(BlocksInChain / 3))
    history2 = applyHeaderChain(history2, fork2.take(BlocksInChain / 2))
    history2.bestHeaderOpt.get shouldBe fork2.take(BlocksInChain / 2).last
    history1.bestHeaderOpt.get shouldBe fork1.last

    val si = history2.syncInfo
    val continuation = history1.continuationIds(si, BlocksInChain * 100)

    fork1.headers.foreach(h => continuation.exists(_._2 == h.id) shouldBe true)

  }

  property("continuationIds() for empty ErgoSyncInfo should contain ids of all headers") {
    var history = genHistory()
    val chain = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)
    history = applyHeaderChain(history, chain)

    val smallerLimit = 2
    val ci0 = history.continuationIds(ErgoSyncInfo(Seq()), smallerLimit)
    ci0.length shouldBe smallerLimit

    chain.headers.take(smallerLimit).map(_.encodedId) shouldEqual ci0.map(c => Algos.encode(c._2))

    val biggerLimit = BlocksInChain + 2
    val ci1 = history.continuationIds(ErgoSyncInfo(Seq()), biggerLimit)
    chain.headers.map(_.id) should contain theSameElementsAs ci1.map(_._2)

    val ci = history.continuationIds(ErgoSyncInfo(Seq()), BlocksInChain)
    ci.foreach(c => c._1 shouldBe Header.modifierTypeId)
    chain.headers.map(_.id) should contain theSameElementsAs ci.map(_._2)
  }

  property("continuationIds() for less developed chain should contain ids of next headers in our chain") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)
    val chain = history.lastHeaders(BlocksInChain)

    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 1 && chain.size > forkLength) {
        val si = ErgoSyncInfo(Seq(chain.headers(chain.size - forkLength - 1).id))
        val continuation = history.continuationIds(si, forkLength)
        continuation.length shouldBe forkLength + 1
        continuation.last._2 shouldEqual chain.last.id
        continuation.head._2 shouldEqual chain.headers(chain.size - forkLength - 1).id
      }
    }
  }

  property("continuationHeaderChains()") {
    var history = genHistory()
    //put 2 blocks
    val inChain = genHeaderChain(2, history, diffBitsOpt = None, useRealTs = false)
    history = applyHeaderChain(history, inChain)
    //apply 2 different forks
    val fork1 = genHeaderChain(2, history, diffBitsOpt = None, useRealTs = false).tail
    val fork2 = genHeaderChain(3, history, diffBitsOpt = None, useRealTs = false).tail
    history = applyHeaderChain(history, fork1)
    history = applyHeaderChain(history, fork2)
    //get continuationHeaderChains
    val continuations = history.continuationHeaderChains(inChain.last, _ => true)
    continuations.length shouldBe 2
    continuations.flatMap(_.tail).map(_.encodedId).toSet should contain theSameElementsAs
      (fork1.headers ++ fork2.headers).map(_.encodedId).toSet
  }

  property("chainToHeader()") {
    var history = genHistory()
    //put 2 blocks
    val inChain = genHeaderChain(2, history, diffBitsOpt = None, useRealTs = false)
    history = applyHeaderChain(history, inChain)
    //apply 2 different forks
    val fork1 = genHeaderChain(2, history, diffBitsOpt = None, useRealTs = false).tail
    val fork2 = genHeaderChain(3, history, diffBitsOpt = None, useRealTs = false).tail
    history = applyHeaderChain(history, fork1)
    history = applyHeaderChain(history, fork2)

    val fork1Chain = history.chainToHeader(None, fork1.last)
    fork1Chain._1 shouldBe None
    fork1Chain._2 shouldEqual HeaderChain(inChain.headers ++ fork1.headers)

    val from1to2Chain = history.chainToHeader(Some(fork1.last), fork2.last)
    from1to2Chain._1.get shouldEqual inChain.last.id
    from1to2Chain._2.headers.map(_.height) shouldEqual fork2.headers.map(_.height)
    from1to2Chain._2.headers shouldEqual fork2.headers

  }

  property("commonBlockThenSuffixes()") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)

    val forkDepth = BlocksInChain / 2
    forAll(smallInt, digest32Gen) { (forkLength: Int, extensionHash: Digest32) =>
      whenever(forkLength > forkDepth) {

        val fork1 = genHeaderChain(forkLength, history, diffBitsOpt = None, useRealTs = false).tail
        val common = fork1.headers(forkDepth)
        val commonInterlinks = history.typedModifierById[Extension](common.extensionId)
          .map(ext => NipopowAlgos.unpackInterlinks(ext.fields).get)
          .getOrElse(Seq.empty)
        val fork2 = fork1.take(forkDepth) ++ genHeaderChain(forkLength + 1, Option(common), commonInterlinks,
          defaultDifficultyControl, extensionHash, diffBitsOpt = None, useRealTs = false)
        val fork1SuffixIds = fork1.headers.drop(forkDepth + 1).map(_.encodedId)
        val fork2SuffixIds = fork2.headers.drop(forkDepth + 1).map(_.encodedId)
        (fork1SuffixIds intersect fork2SuffixIds) shouldBe empty

        history = applyHeaderChain(history, fork1)
        history.bestHeaderOpt.get shouldBe fork1.last

        val (our, their) = history.commonBlockThenSuffixes(fork2, history.bestHeaderOpt.get, 1000)
        our.head shouldBe their.head
        our.head shouldBe common
        our.last shouldBe fork1.last
        their.last shouldBe fork2.last
      }
    }
  }

  property("Append headers to best chain in history") {
    var history = genHistory()

    val chain = genHeaderChain(BlocksInChain, history, diffBitsOpt = None, useRealTs = false)

    chain.headers.foreach { header =>
      val inHeight = history.heightOf(header.parentId).getOrElse(ErgoHistory.EmptyHistoryHeight)

      history.contains(header) shouldBe false
      history.applicable(header) shouldBe true

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.applicable(header) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.openSurfaceIds() shouldEqual Seq(header.id)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }

  property("bestHeadersAfter returns correct number of headers") {
    val chainLength = 50

    var history = genHistory()
    val chain = genHeaderChain(chainLength, history, diffBitsOpt = None, useRealTs = false)

    history = applyHeaderChain(history, chain)

    val suffixLength = Random.nextInt(chainLength - 1) + 1
    val hdr = chain.headers.takeRight(suffixLength).head
    val count = Random.nextInt(suffixLength)
    history.bestHeadersAfter(hdr, count).length shouldBe count

    history.bestHeadersAfter(chain.last, 0).length shouldBe 0
    history.bestHeadersAfter(chain.last, 1).length shouldBe 0
    history.bestHeadersAfter(chain.last, Int.MaxValue).length shouldBe 0
  }

}
