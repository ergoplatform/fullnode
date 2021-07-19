package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{Extension, Header, HeaderChain}
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.ErgoModifiersCache
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity.{Absent, Invalid, Unknown, Valid}
import scorex.testkit.utils.NoShrink

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class VerifyADHistorySpecification extends HistoryTestHelpers with NoShrink {

  type PM = ErgoPersistentModifier

  private def genHistory(blocksNum: Int = 0,
                         minFullHeight: Option[Int] = Some(ErgoHistory.GenesisHeight)): (ErgoHistory, Seq[ErgoFullBlock]) = {
    val inHistory = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep)
    minFullHeight.foreach { h =>
      inHistory.pruningProcessor.minimalFullBlockHeightVar = h
      inHistory.pruningProcessor.isHeadersChainSyncedVar = true
    }

    if (blocksNum > 0) {
      val chain = genChain(blocksNum, inHistory)
      (applyChain(inHistory, chain), chain)
    } else {
      (inHistory, Seq.empty)
    }
  }

  property("Forks that include genesis block") {
    var (history, _) = genHistory()
    val fork1 = genChain(3, history)
    val fork2 = genChain(2, history)
    val fork3 = genChain(4, history)

    // apply 3 headers long chain
    history = applyChain(history, fork1)
    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get shouldBe fork1.last.header

    // apply 2 headers long chain, should stay on previous one
    history = applyChain(history, fork2)
    history.bestFullBlockOpt.get.header shouldBe history.bestHeaderOpt.get
    history.bestHeaderOpt.get shouldBe fork1.last.header

    // apply 4 headers long chain, should update chain
    history = applyChain(history, fork3)
    history.bestHeaderOpt.get shouldBe fork3.last.header
    history.bestFullBlockOpt.get.header.height shouldBe fork3.last.header.height

  }


  property("ErgoModifiersCache.findCandidateKey() should find headers in case of forks") {
    val modifiersCache = new ErgoModifiersCache(Int.MaxValue)

    var (history, _) = genHistory(2)

    val fork1 = genChain(2, history).tail
    val fork2 = genChain(3, history).tail

    history = applyChain(history, fork1)

    fork2.foreach { fb =>
      modifiersCache.put(fb.header.id, fb.header)
    }
    history.applicable(fork2.head.header) shouldBe true
    modifiersCache.contains(fork2.head.header.id) shouldBe true
    modifiersCache.findCandidateKey(history).isDefined shouldBe true
  }

  property("should not be able to apply blocks older than blocksToKeep") {
    var history = genHistory()._1
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.get shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    val fullBlocksToApply = chain.tail
    history.pruningProcessor.updateBestFullBlock(fullBlocksToApply(BlocksToKeep - 1).header)

    history.applicable(chain.head.blockTransactions) shouldBe false

    history = applyBlock(history, fullBlocksToApply.head)
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header

    history.applicable(chain.head.blockTransactions) shouldBe false

    fullBlocksToApply.tail.foreach { f =>
      history = applyBlock(history, f)
    }
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.last.header

    //block sections should be already pruned
    fullBlocksToApply.head.header.sectionIds.foreach(id => history.contains(id._2) shouldBe false)

    //block transactions should not be able to apply since they are too far back in history
    fullBlocksToApply.head.blockSections.foreach(s => history.applicable(s) shouldBe false)
  }

  property("proofs and transactions application in random order with forks") {
    forAll(smallInt, positiveLongGen) { (chainHeight, seed) =>
      whenever(chainHeight > 0) {
        val (history, chain) = genHistory(1)
        val r = new Random(seed)
        val genesis = chain.head
        history.bestFullBlockOpt shouldBe Some(genesis)

        val chains = Seq(genChain(chainHeight, genesis), genChain(chainHeight + 1, genesis)).map(_.tail)
        chains.foreach(chain => applyHeaderChain(history, HeaderChain(chain.map(_.header))))
        val indices: Seq[(Int, Int)] = chains.indices.flatMap { chainIndex =>
          val chain = chains(chainIndex)
          chain.indices.map(blockIndex => (chainIndex, blockIndex))
        }

        var appended: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty

        def findBestBlock(appendedToCheck: Seq[ErgoFullBlock]): ErgoFullBlock = {
          def firstInAppended(h: Header): Header = {
            appended.find(_.header.id == h.parentId).map(_.header) match {
              case Some(prev) => firstInAppended(prev)
              case None => h
            }
          }

          if (appendedToCheck.isEmpty) {
            genesis
          } else {
            val best = appendedToCheck.maxBy(_.header.height)
            if (firstInAppended(best.header).parentId == genesis.id) {
              best
            } else {
              findBestBlock(appendedToCheck.filterNot(_.id == best.id))
            }
          }
        }

        r.shuffle(indices).foreach { i =>
          val block = chains(i._1)(i._2)
          val sectionsToAppend = block.blockSections.filterNot(_.modifierTypeId == Extension.modifierTypeId)
          r.shuffle(sectionsToAppend).foreach(s => history.append(s) shouldBe 'success)

          appended += block

          sectionsToAppend.forall(history.contains) shouldBe true
        }
      }
    }
  }

  property("apply proofs that link incomplete chain") {
    var history = genHistory()._1
    val chain = genChain(4)

    val block0 = chain.head
    val block1 = chain(1)
    val block2 = chain(2)
    val block3 = chain(3)

    applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestFullBlockOpt shouldBe None
    history.bestHeaderOpt shouldBe Some(block3.header)

    history = applySection(history, block0.adProofs.get)
    history.contains(block0.adProofs.get.id) shouldBe true

    history = applySection(history, block2.adProofs.get)
    history.contains(block2.adProofs.get.id) shouldBe true

    history = applySection(history, block3.adProofs.get)
    history.contains(block3.adProofs.get.id) shouldBe true

    history = applySection(history, block1.adProofs.get)
    history.contains(block1.adProofs.get.id) shouldBe true

    history = applyBlock(history, block0)
    history = applyBlock(history, block1)
    history = applyBlock(history, block2)
    history = applyBlock(history, block3)

    history.bestFullBlockOpt shouldBe Some(block3)
  }

  property("bootstrap from headers and last full blocks") {
    var history = genHistory()._1
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None
    history.pruningProcessor.updateBestFullBlock(chain.last.header)

    val fullBlocksToApply = chain.takeRight(BlocksToKeep)

    history = applyBlock(history, fullBlocksToApply.head)
    history.bestFullBlockOpt.value.header shouldBe fullBlocksToApply.head.header
  }

  property("syncInfo()") {
    val (history, chain) = genHistory(BlocksInChain)

    val si = history.syncInfo.asInstanceOf[ErgoSyncInfoV1]
    si.lastHeaderIds.last shouldEqual chain.last.header.id
  }

  property("reportModifierIsValid should set isSemanticallyValid() result") {
    var history = genHistory(1)._1
    history.bestFullBlockOpt.isDefined shouldBe true

    val chain = genChain(BlocksInChain, history).tail
    chain.head.parentId shouldEqual history.bestFullBlockOpt.value.id

    chain.foreach { fullBlock =>
      history.bestHeaderOpt.foreach(b => b.id shouldEqual fullBlock.parentId)
      history.bestFullBlockOpt.foreach(b => b.header shouldBe history.bestHeaderOpt.value)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Absent
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Absent)

      history = applyBlock(history, fullBlock)

      history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      history.bestHeaderOpt.value.id shouldEqual fullBlock.header.id

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Unknown)

      history.reportModifierIsValid(fullBlock.header)
      fullBlock.blockSections.foreach(s => history.reportModifierIsValid(s))

      history.reportModifierIsValid(fullBlock)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Valid
      fullBlock.blockSections.foreach(s => history.isSemanticallyValid(s.id) shouldBe Valid)
    }
  }

  property("reportModifierIsInvalid should set isSemanticallyValid() result for all linked modifiers") {
    var history = genHistory(1)._1

    history.bestFullBlockOpt should not be None

    val chain = genChain(BlocksInChain, history.bestFullBlockOpt.get).tail
    (chain.head.header.parentId == Header.GenesisParentId) shouldBe false

    history = applyChain(history, chain)

    chain.reverse.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Unknown
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Unknown


      val progressInfo = ProgressInfo[PM](Option(fullBlock.header.parentId), Seq(fullBlock), Seq.empty, Seq.empty)
      history.reportModifierIsInvalid(fullBlock.header, progressInfo)

      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportModifierIsInvalid should mark invalid all forks containing this header") {
    var (history, inChain) = genHistory(2)

    val fork1 = genChain(3, history).tail
    val fork2 = genChain(3, history).tail
    fork1.head.parentId shouldEqual fork2.head.parentId

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    val progressInfo = ProgressInfo[PM](Some(inChain.last.parentId), fork2, Seq.empty, Seq.empty)
    history.reportModifierIsInvalid(inChain.last.header, progressInfo)

    fork1.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }

    fork2.foreach { fullBlock =>
      history.isSemanticallyValid(fullBlock.header.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.adProofs.value.id) shouldBe Invalid
      history.isSemanticallyValid(fullBlock.blockTransactions.id) shouldBe Invalid
    }
  }

  property("reportModifierIsInvalid should return blocks to rollback and to process") {
    var history = genHistory(3)._1
    val common = history.bestFullBlockOpt.value

    val fork1 = genChain(3, common).tail
    val fork2 = genChain(2, common).tail

    history = applyChain(history, fork1)
    history = applyChain(history, fork2)

    history.bestHeaderOpt.value shouldBe fork1.last.header

    val progressInfo = ProgressInfo[PM](Some(common.parentId), fork1, Seq.empty, Seq.empty)
    history.reportModifierIsInvalid(fork1.head.header, progressInfo)

    history.bestHeaderOpt.value shouldBe fork2.last.header
    history.bestFullBlockOpt.value shouldBe fork2.last
  }

  property("reportModifierIsInvalid for non-last block in best chain without better forks") {
    var (history, chain) = genHistory(BlocksInChain)

    history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
    history.bestHeaderOpt.value shouldEqual chain.last.header

    val invalidChain = chain.takeRight(2)

    val progressInfo = ProgressInfo[PM](Some(invalidChain.head.parentId), invalidChain, Seq.empty, Seq.empty)
    val report = history.reportModifierIsInvalid(invalidChain.head.header, progressInfo)
    history = report._1
    val processInfo = report._2
    processInfo.toApply.isEmpty shouldBe true
    processInfo.branchPoint.value shouldEqual invalidChain.head.header.parentId
    processInfo.toRemove shouldEqual invalidChain

    history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
    history.bestHeaderOpt.value.id shouldEqual invalidChain.head.parentId
  }

  property("Report invalid for best full block") {
    val (history, chain) = genHistory(BlocksInChain)

    chain.takeRight(BlocksToKeep - 2).reverse.foreach { fullBlock =>
      history.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      history.bestHeaderOpt.value shouldEqual fullBlock.header

      val parentHeader = history.typedModifierById[Header](fullBlock.header.parentId).value
      history.contains(parentHeader.transactionsId) shouldBe true
      history.contains(parentHeader.ADProofsId) shouldBe true

      val progressInfo = ProgressInfo[PM](Some(parentHeader.id), Seq(fullBlock), Seq.empty, Seq.empty)
      val (repHistory, _) = history.reportModifierIsInvalid(fullBlock.blockTransactions, progressInfo)
      repHistory.bestFullBlockOpt.value.header shouldBe history.bestHeaderOpt.value
      repHistory.bestHeaderOpt.value shouldBe parentHeader
    }
  }

  property("prune old blocks test") {
    val blocksToPrune = 20

    val (history, chain) = genHistory(BlocksToKeep + blocksToPrune + 1)

    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt.value.header shouldBe chain.last.header

    //genesis block is not pruned
    chain.take(blocksToPrune).tail.foreach { b =>
      history.modifierById(b.header.transactionsId) shouldBe None
      history.modifierById(b.header.ADProofsId) shouldBe None
    }

    chain.takeRight(BlocksToKeep).foreach { b =>
      history.modifierById(b.header.transactionsId).isDefined shouldBe true
      history.modifierById(b.header.ADProofsId).isDefined shouldBe true
    }
  }

  property("process fork from genesis") {
    var (history, c) = genHistory(1)
    val genesis = c.head
    val fork1 = genChain(1, history.bestFullBlockOpt.value).tail
    val fork2 = genChain(2, history.bestFullBlockOpt.value).tail

    history = applyChain(history, fork1)
    history.bestHeaderOpt.value shouldBe fork1.last.header

    history = applyChain(history, fork2.dropRight(1))
    val lastBlock = fork2.last
    history = history.append(lastBlock.header).get._1
      .append(lastBlock.blockTransactions).get._1
      .append(lastBlock.extension).get._1

    val changes = history.append(lastBlock.adProofs.value).get
    history = changes._1
    history.bestHeaderOpt.value shouldBe fork2.last.header

    val processInfo = changes._2
    processInfo.branchPoint.get shouldEqual genesis.id
    processInfo.toRemove should contain theSameElementsAs fork1
    processInfo.toApply should contain theSameElementsAs fork2

  }

  property("process fork from existing chain") {
    var history = genHistory(BlocksInChain)._1

    history.bestFullBlockOpt.isDefined should not be None
    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 0) {
        val branchPoint = history.bestFullBlockOpt.value
        val fork1 = genChain(forkLength, branchPoint).tail
        val fork2 = genChain(forkLength + 1, branchPoint).tail

        history = applyChain(history, fork1)
        history.bestHeaderOpt.value shouldBe fork1.last.header

        history = applyChain(history, fork2.dropRight(1))
        val lastBlock = fork2.last
        history = history.append(lastBlock.header).get._1
          .append(lastBlock.extension).get._1
          .append(lastBlock.blockTransactions).get._1

        val changes = history.append(lastBlock.adProofs.value).get
        history = changes._1
        history.bestHeaderOpt.value shouldBe fork2.last.header

        val processInfo = changes._2
        processInfo.branchPoint.value shouldEqual branchPoint.id
        processInfo.toRemove should contain theSameElementsAs fork1
        processInfo.toApply should contain theSameElementsAs fork2
      }
    }
  }

  property("Appended full blocks to best chain in full history") {
    var history = genHistory(1)._1
    history.bestFullBlockOpt.nonEmpty shouldBe true

    val chain = genChain(BlocksInChain, history).tail
    chain.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.value
      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val proofs = fullBlock.adProofs.value
      val extension = fullBlock.extension
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(proofs) shouldBe false
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(txs) shouldBe true
      history.applicable(extension) shouldBe true
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock
      history.openSurfaceIds().head shouldEqual startFullBlock.header.id

      history = history.append(txs).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe true
      history.applicable(extension) shouldBe true
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.bestFullBlockOpt.get shouldBe startFullBlock

      history = history.append(proofs).get._1
      history = history.append(extension).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(proofs) shouldBe true
      history.contains(extension) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(proofs) shouldBe false
      history.applicable(extension) shouldBe false
      history.applicable(txs) shouldBe false
      history.bestHeaderOpt.value shouldBe header
      history.bestFullBlockOpt.value shouldBe fullBlock
      history.openSurfaceIds().head shouldEqual fullBlock.header.id
    }
  }

}
