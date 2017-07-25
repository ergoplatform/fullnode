package org.ergoplatform.nodeView.history

import java.io.File

import io.circe.Json
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers

class HistoryTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  var fullHistory = generateHistory(verify = true, adState = true)
  var lightHistory = generateHistory(verify = false, adState = true)
  assert(fullHistory.bestFullBlockId.isDefined)
  assert(lightHistory.bestFullBlockId.isEmpty)


  val BlocksInChain = 30

  property("commonBlockThenSuffixes()") {
    var history = lightHistory
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 10) {

        val fork1 = genHeaderChain(forkLength, Seq(history.bestHeader)).tail
        val common = fork1.headers(10)
        val fork2 = fork1.take(10) ++ genHeaderChain(forkLength + 1, Seq(common))

        history = applyHeaderChain(history, fork1)
        history.bestHeader shouldBe fork1.last

        val (our, their) = history.commonBlockThenSuffixes(fork2, history.bestHeader)
        our.head shouldBe their.head
        our.head shouldBe common
        our.last shouldBe fork1.last
        their.last shouldBe fork2.last
      }
    }
  }

  property("process fork") {
    var history = fullHistory
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val fork1 = genChain(forkLength, Seq(history.bestFullBlockOpt.get)).tail
        val fork2 = genChain(forkLength + 1, Seq(history.bestFullBlockOpt.get)).tail

        history = applyChain(history, fork1)
        history.bestHeader shouldBe fork1.last.header

        history = applyChain(history, fork2)
        history.bestHeader shouldBe fork2.last.header
      }
    }
  }

  property("Append headers to best chain in history") {
    var history = lightHistory
    val chain = genHeaderChain(BlocksInChain, Seq(history.bestHeader)).tail
    chain.headers.foreach { header =>
      val inHeight = history.heightOf(header.parentId).get
      history.contains(header) shouldBe false
      history.applicable(header) shouldBe true

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.applicable(header) shouldBe false
      history.bestHeader shouldBe header
      history.openSurfaceIds() shouldEqual Seq(header.id)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }

  property("Appended full blocks to best chain in full history") {
    assert(fullHistory.bestFullBlockOpt.get.header == fullHistory.bestHeader)
    val chain = genChain(BlocksInChain, Seq(fullHistory.bestFullBlockOpt.get)).tail
    chain.foreach { fullBlock =>
      val startFullBlock = fullHistory.bestFullBlockOpt.get
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val proofs = fullBlock.aDProofs.get
      fullHistory.contains(header) shouldBe false
      fullHistory.contains(txs) shouldBe false
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe true
      fullHistory.applicable(proofs) shouldBe false
      fullHistory.applicable(txs) shouldBe false

      fullHistory = fullHistory.append(header).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe false
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe true
      fullHistory.applicable(txs) shouldBe true
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe startFullBlock
      fullHistory.openSurfaceIds().head shouldEqual startFullBlock.header.id

      fullHistory = fullHistory.append(txs).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe true
      fullHistory.contains(proofs) shouldBe false
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe true
      fullHistory.applicable(txs) shouldBe false
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe startFullBlock

      fullHistory = fullHistory.append(proofs).get._1

      fullHistory.contains(header) shouldBe true
      fullHistory.contains(txs) shouldBe true
      fullHistory.contains(proofs) shouldBe true
      fullHistory.applicable(header) shouldBe false
      fullHistory.applicable(proofs) shouldBe false
      fullHistory.applicable(txs) shouldBe false
      fullHistory.bestHeader shouldBe header
      fullHistory.bestFullBlockOpt.get shouldBe fullBlock
      fullHistory.openSurfaceIds().head shouldEqual fullBlock.header.id
    }
  }

  property("compare()") {
    //TODO what if headers1 > headers2 but fullchain 1 < fullchain2?
  }

  property("continuationIds()") {
    //TODO
  }

  property("syncInfo()") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
        val answer = Random.nextBoolean()
        history = applyChain(history, chain)
        val si = history.syncInfo(answer)
        si.answer shouldBe answer
        si.lastBlockIds.flatten shouldEqual history.lastBlocks(Math.max(ErgoSyncInfo.MaxBlockIds, history.fullBlocksHeight)).flatMap(_.id)
    */
  }

  property("lastBlocks() should return last blocks") {
    /*
        val blocksToApply = BlocksInChain
        val chain = genChain(blocksToApply, Seq(history.bestFullBlock)).tail
        history = applyChain(history, chain)
        history.fullBlocksHeight should be > blocksToApply
        val lastBlocks = history.lastBlocks(blocksToApply)
        lastBlocks.length shouldBe blocksToApply
        lastBlocks.foreach(b => assert(chain.contains(b)))
        lastBlocks.last shouldBe history.bestFullBlock
    */
  }

  property("Drop last block from history") {
    /*
        val chain = genChain(BlocksInChain, Seq(history.bestFullBlock)).tail
        chain.foreach { block =>
          val header = block.header
          val inBestBlock = history.bestFullBlock

          history.bestHeader shouldBe inBestBlock.header
          history.bestFullBlock shouldBe inBestBlock

          history = history.append(header).get._1.append(block).get._1

          history.bestHeader shouldBe header
          history.bestFullBlock shouldBe block

          history = history.drop(header.id)

          history.bestHeader shouldBe inBestBlock.header
          history.bestFullBlock shouldBe inBestBlock

          history = history.append(header).get._1.append(block).get._1

        }
    */
  }

  private def generateHistory(verify: Boolean, adState: Boolean): ErgoHistory = {
    val fullHistorySettings: ErgoSettings = new ErgoSettings {
      override def settingsJSON: Map[String, Json] = Map()

      override val verifyTransactions: Boolean = verify
      override val ADState: Boolean = adState
      override val dataDir: String = s"/tmp/ergo/test-history-$verify"
    }
    new File(fullHistorySettings.dataDir).mkdirs()
    ErgoHistory.readOrGenerate(fullHistorySettings)
  }

  private def historyTest(histories: Seq[ErgoHistory])(fn: ErgoHistory => Unit): Unit = histories.foreach(fn)


}
