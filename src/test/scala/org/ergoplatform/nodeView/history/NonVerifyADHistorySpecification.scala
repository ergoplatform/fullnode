package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{Header, HeaderChain}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.core.consensus.History._
import scorex.crypto.hash.Digest32

class NonVerifyADHistorySpecification extends HistoryTestHelpers {

  private def genHistory() =
    generateHistory(verifyTransactions = false, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 0, epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private lazy val popowHistory = ensureMinimalHeight(genHistory(), 100)

  property("Header votes") {
    import org.ergoplatform.settings.Parameters._

    val history = genHistory()

    //double vote
    val wrongVotes1 = Array(StorageFeeFactorIncrease, StorageFeeFactorIncrease, NoParameter)
    val hw1 = genHeaderChain(1, history).head.copy(votes = wrongVotes1, version = 0: Byte)
    history.applicableTry(hw1).isSuccess shouldBe false


    //contradictory votes
    val wrongVotes2 = Array(StorageFeeFactorIncrease, StorageFeeFactorDecrease, NoParameter)
    val hw2 = genHeaderChain(1, history).head.copy(votes = wrongVotes2, version = 0: Byte)
    history.applicableTry(hw2).isSuccess shouldBe false

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(StorageFeeFactorIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    val hw3 = genHeaderChain(1, history).head.copy(votes = wrongVotes3, version = 0: Byte)
    history.applicableTry(hw3).isSuccess shouldBe false

    //a vote proposed on non-existing parameter
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    val hw4 = genHeaderChain(1, history).head.copy(votes = wrongVotes4, version = 0: Byte, height = 2)
    history.applicableTry(hw4).isSuccess shouldBe false
  }

  property("Should calculate difficulty correctly") {
    val epochLength = 3
    val useLastEpochs = 3

    var history = generateHistory(verifyTransactions = false, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 0,
      epochLength = epochLength, useLastEpochs = useLastEpochs)
    val blocksBeforeRecalculate = epochLength + 1

    history = applyHeaderChain(history, genHeaderChain(blocksBeforeRecalculate, history))
    history.requiredDifficultyAfter(history.bestHeaderOpt.get) shouldBe Constants.InitialDifficulty
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
    val common = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history)
    val fork2 = genHeaderChain(BlocksInChain + 1, history)

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

    val common = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history)
    val fork2 = genHeaderChain(BlocksInChain + 1, history)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last

    history.compare(getInfo(fork2)) shouldBe Younger
    history.compare(getInfo(fork1)) shouldBe Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe Younger
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe Younger
    history.compare(getInfo(fork2.tail)) shouldBe Older
  }

  property("continuationIds() on forks") {
    var history1 = genHistory()
    var history2 = genHistory()
    val inChain = genHeaderChain(2, history1)


    //put genesis
    history1 = applyHeaderChain(history1, inChain)
    history2 = applyHeaderChain(history2, inChain)
    val fork1 = genHeaderChain(BlocksInChain, history1).tail
    val fork2 = genHeaderChain(BlocksInChain, history1).tail

    //apply 2 different forks
    history1 = applyHeaderChain(history1, fork1)
    history2 = applyHeaderChain(history2, fork1.take(BlocksInChain / 3))
    history2 = applyHeaderChain(history2, fork2.take(BlocksInChain / 2))
    history2.bestHeaderOpt.get shouldBe fork2.take(BlocksInChain / 2).last
    history1.bestHeaderOpt.get shouldBe fork1.last

    val si = history2.syncInfo
    val continuation = history1.continuationIds(si, BlocksInChain * 100).get

    fork1.headers.foreach(h => continuation.exists(_._2 == h.id) shouldBe true)

  }

  property("continuationIds() for empty ErgoSyncInfo should contain ids of all headers") {
    var history = genHistory()
    val chain = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, chain)

    val smallerLimit = 2
    val ci0 = history.continuationIds(ErgoSyncInfo(Seq()), smallerLimit).get
    ci0.length shouldBe smallerLimit

    chain.headers.take(smallerLimit).map(_.encodedId) shouldEqual ci0.map(c => Algos.encode(c._2))

    val biggerLimit = BlocksInChain + 2
    val ci1 = history.continuationIds(ErgoSyncInfo(Seq()), biggerLimit).get
    chain.headers.map(_.id) should contain theSameElementsAs ci1.map(_._2)

    val ci = history.continuationIds(ErgoSyncInfo(Seq()), BlocksInChain).get
    ci.foreach(c => c._1 shouldBe Header.modifierTypeId)
    chain.headers.map(_.id) should contain theSameElementsAs ci.map(_._2)
  }

  property("continuationIds() for light history should contain ids of next headers in our chain") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)
    val chain = history.lastHeaders(BlocksInChain)

    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 1 && chain.size > forkLength) {
        val si = ErgoSyncInfo(Seq(chain.headers(chain.size - forkLength - 1).id))
        val continuation = history.continuationIds(si, forkLength).get
        continuation.length shouldBe forkLength
        continuation.last._2 shouldEqual chain.last.id
        continuation.head._2 shouldEqual chain.headers(chain.size - forkLength).id
      }
    }
  }

  property("continuationHeaderChains()") {
    var history = genHistory()
    //put 2 blocks
    val inChain = genHeaderChain(2, history)
    history = applyHeaderChain(history, inChain)
    //apply 2 different forks
    val fork1 = genHeaderChain(2, history).tail
    val fork2 = genHeaderChain(3, history).tail
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
    val inChain = genHeaderChain(2, history)
    history = applyHeaderChain(history, inChain)
    //apply 2 different forks
    val fork1 = genHeaderChain(2, history).tail
    val fork2 = genHeaderChain(3, history).tail
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

        val fork1 = genHeaderChain(forkLength, history).tail
        val common = fork1.headers(forkDepth)
        val fork2 = fork1.take(forkDepth) ++ genHeaderChain(forkLength + 1, Option(common),
          defaultDifficultyControl, extensionHash)
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

    val chain = genHeaderChain(BlocksInChain, history)

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

}
