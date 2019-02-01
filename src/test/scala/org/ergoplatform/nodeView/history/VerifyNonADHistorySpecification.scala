package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistoryTestHelpers

class VerifyNonADHistorySpecification extends HistoryTestHelpers {

  private def genHistory(blocksToKeep: Int = BlocksToKeep) =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep)

  property("block sections application in incorrect order") {
    var history = genHistory()
    val chain = genChain(6, history)
    if (!history.pruningProcessor.isHeadersChainSynced) {
      history.pruningProcessor.updateBestFullBlock(chain.last.header)
    }
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    chain.foreach(fb => history.append(fb.extension).get)

    history = history.append(chain.tail.head.blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    val pi1 = history.append(chain.head.blockTransactions).get._2
    history.bestFullBlockOpt.value shouldBe chain.tail.head
    pi1.toApply.length shouldBe 2

    chain.tail.tail.tail.foreach(c => history.append(c.blockTransactions))
    history.bestFullBlockOpt.value.header.height shouldBe chain.tail.head.header.height

    val pi = history.append(chain.tail.tail.head.blockTransactions).get._2
    val expected = chain.tail.tail
    pi.toApply.map(_.asInstanceOf[ErgoFullBlock]) shouldBe expected
  }

  property("bootstrap from headers and last full blocks") {
    var history = genHistory()
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    if (!history.pruningProcessor.isHeadersChainSynced) {
      history.pruningProcessor.updateBestFullBlock(chain.last.header)
    }

    // Until UTXO snapshot synchronization is implemented, we should always start to apply full blocks from genesis
    val fullBlocksToApply = chain

    history = history.append(fullBlocksToApply.head.blockTransactions).get._1
    history = history.append(fullBlocksToApply.head.extension).get._1
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header
  }

  property("nextModifiersToDownload") {
    var history = genHistory(blocksToKeep = -1)
    val chain = genChain(BlocksToKeep)
    history = applyBlock(history, chain.head)
    history.bestFullBlockOpt.value shouldBe chain.head
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header).tail))

    val missedChain = chain.tail.toList
    val missedBS = missedChain.flatMap(fb => Seq((BlockTransactions.modifierTypeId, fb.blockTransactions.encodedId),
      (Extension.modifierTypeId, fb.extension.encodedId)))

    history.nextModifiersToDownload(1, _ => true).map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBS.take(1)

    history.nextModifiersToDownload(2 * (BlocksToKeep - 1), _ => true)
      .map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBS

    history.nextModifiersToDownload(2, id => id != missedChain.head.blockTransactions.id)
      .map(id => (id._1, Algos.encode(id._2))) shouldEqual missedBS.tail.take(2)
  }

  property("append header as genesis") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history).head
    val updHistory = history.append(header).get._1
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header as genesis - via applyHeaderChain") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history).head

    val updHistory = applyHeaderChain(history, HeaderChain(Seq(header)))
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header to genesis - 2") {
    val (us, bh) = createUtxoState()

    val block = validFullBlock(None, us, bh)

    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = block.header

    HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)).get shouldBe header

    val actualHeader = history.append(header).get._1.bestHeaderOpt.value
    actualHeader shouldBe header
  }

  property("Appended headers and transactions blocks to best chain in tx history") {
    var history = genHistory()

    history = applyChain(history, genChain(BlocksInChain, history))

    genChain(BlocksInChain, history).tail.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.value

      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val extension = fullBlock.extension
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe true
      history.applicable(extension) shouldBe true
      history.bestHeaderOpt.value shouldBe header

      history.bestFullBlockOpt.value shouldBe startFullBlock

      history.openSurfaceIds().head shouldBe startFullBlock.header.id

      history = history.append(txs).get._1
      history = history.append(extension).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(extension) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false
      history.bestHeaderOpt.value shouldBe header
      history.bestFullBlockOpt.value.header shouldBe fullBlock.header
    }
  }

}
