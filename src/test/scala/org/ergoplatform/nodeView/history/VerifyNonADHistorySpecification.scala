package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{FullBlockProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.core.ModifierTypeId
import scorex.core.consensus.History.ProgressInfo

class VerifyNonADHistorySpecification extends HistoryTestHelpers {
  import ToDownloadProcessor._

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  property("block sections application in incorrect order") {
    var history = genHistory()
    val chain = genChain(6, history)
    if (!history.pruningProcessor.isHeadersChainSynced) {
      history.pruningProcessor.updateBestFullBlock(chain.last.header)
    }
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    chain.foreach(fb => history.append(fb.extension).get)

    history = history.append(chain(1).blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    val pi1 = history.append(chain(0).blockTransactions).get._2
    history.bestFullBlockOpt.value shouldBe chain(1)
    pi1.toApply.length shouldBe 2

    chain.drop(3).foreach(c => history.append(c.blockTransactions))
    history.bestFullBlockOpt.value.header.height shouldBe chain(1).header.height

    val (hi, pi) = history.append(chain(2).blockTransactions).get
    val expected = chain.drop(2)

    expected.forall(b => hi.asInstanceOf[FullBlockProcessor].isInBestFullChain(b.id)) shouldBe true

    pi.toApply.map(_.asInstanceOf[ErgoFullBlock]) shouldBe expected
  }

  property("full chain status updating") {

    def isInBestChain(b: ErgoFullBlock, h: ErgoHistory): Boolean = {
      h.asInstanceOf[FullBlockProcessor].isInBestFullChain(b.id)
    }

    var history = genHistory()
    val initChain = genChain(6, history)

    val stableChain = initChain.take(3)
    val altChain = genChain(8, stableChain.last).tail

    // apply initial initChain (1 to 6)
    history = applyChain(history, initChain)

    history.bestFullBlockIdOpt.get shouldEqual initChain.last.id
    initChain.forall(b => isInBestChain(b, history)) shouldBe true

    // apply better initChain forking initial one (1 to 3 (init initChain), 3 to 11 (new initChain))
    history = applyChain(history, altChain)

    history.bestFullBlockIdOpt.get shouldEqual altChain.last.id
    // first blocks from init chain are still marked as best chain
    stableChain.forall(b => isInBestChain(b, history)) shouldBe true
    // other blocks from init chain are no more in best chain
    initChain.drop(3).forall(b => !isInBestChain(b, history)) shouldBe true
    // all blocks from fork are marked as best chain
    altChain.forall(b => isInBestChain(b, history)) shouldBe true

    val invalidChainHead = altChain.head

    // invalidate modifier from fork
    history.reportModifierIsInvalid(invalidChainHead.blockTransactions,
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty))

    history.bestFullBlockIdOpt.get shouldEqual initChain.last.id

    // all blocks from init chain are marked as best chain again
    initChain.forall(b => isInBestChain(b, history)) shouldBe true
    // blocks from fork no longer marked as best chain
    altChain.forall(b => !isInBestChain(b, history)) shouldBe true
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
    var history = genHistory()
    val chain = genChain(BlocksToKeep)
    history = applyBlock(history, chain.head)
    history.bestFullBlockOpt.value shouldBe chain.head
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header).tail))

    val missedChain = chain.tail.toList
    val missedBS = missedChain.flatMap { fb =>
      Seq((BlockTransactions.modifierTypeId, fb.blockTransactions.encodedId), (Extension.modifierTypeId, fb.extension.encodedId))
    }.foldLeft(Map.empty[ModifierTypeId, Seq[String]]) { case (newAcc, (mType, mId)) =>
      newAcc.adjust(mType)(_.fold(Seq(mId))(_ :+ mId))
    }

    history.nextModifiersToDownload(1, id => !history.contains(id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS.mapValues(_.take(1)).view.force

    history.nextModifiersToDownload(2 * (BlocksToKeep - 1), id => !history.contains(id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS

    history.nextModifiersToDownload(2, id => !history.contains(id) && (id != missedChain.head.blockTransactions.id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS.mapValues(_.take(2).filter( _ != missedChain.head.blockTransactions.id)).view.force
  }

  property("append header as genesis") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history, diffBitsOpt = None, useRealTs = false).head
    val updHistory = history.append(header).get._1
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header as genesis - via applyHeaderChain") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history, diffBitsOpt = None, useRealTs = false).head

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

    HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)) shouldBe header

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
