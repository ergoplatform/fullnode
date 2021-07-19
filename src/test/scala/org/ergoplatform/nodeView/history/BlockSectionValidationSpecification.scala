package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.core.block.Block.Version
import scorex.core.consensus.ModifierSemanticValidity
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import scorex.util.encode.Base16

class BlockSectionValidationSpecification extends HistoryTestHelpers {
  
  private def changeProofByte(version: Version, outcome: Symbol) = {
    val (history, block) = init(version)
    val bt = block.blockTransactions
    val txBytes = HistoryModifierSerializer.toBytes(bt)

    val txs = bt.transactions
    val proof = txs.head.inputs.head.spendingProof.proof
    proof(0) = if(proof.head < 0) (proof.head + 1).toByte else (proof.head - 1).toByte

    val txBytes2 = HistoryModifierSerializer.toBytes(bt)

    val hashBefore = Base16.encode(Blake2b256(txBytes))
    val hashAfter = Base16.encode(Blake2b256(txBytes2))

    val wrongBt = HistoryModifierSerializer.parseBytes(txBytes2).asInstanceOf[BlockTransactions]

    hashBefore should not be hashAfter
    history.applicableTry(bt) shouldBe 'success
    history.applicableTry(wrongBt) shouldBe outcome
  }

  property("BlockTransactions - proof byte changed - v.1") {
    changeProofByte(Header.InitialVersion, outcome = 'success)
  }

  property("BlockTransactions - proof byte changed - v.2") {
    changeProofByte((Header.InitialVersion + 1).toByte, outcome = 'failure)
  }

  property("BlockTransactions commons check") {
    val (history, block) = init()
    commonChecks(history, block.blockTransactions, block.header)
  }

  property("ADProofs validation") {
    val (history, block) = init()
    commonChecks(history, block.adProofs.get, block.header)
  }

  property("Extension validation") {
    val (history, block) = init()
    commonChecks(history, block.extension, block.header)
  }

  private def init(version: Version = Header.InitialVersion) = {
    var history = genHistory()
    val chain = genChain(2, history, version)
    history = applyBlock(history, chain.head)
    history = history.append(chain.last.header).get._1
    (history, chain.last)
  }

  private def commonChecks(history: ErgoHistory, section: BlockSection, header: Header) = {
    history.applicableTry(section) shouldBe 'success
    // header should contain correct digest
    history.applicableTry(withUpdatedHeaderId(section, section.id)) shouldBe 'failure

    // should not be able to apply when blocks at this height are already pruned
    history.applicableTry(section) shouldBe 'success
    history.pruningProcessor.minimalFullBlockHeightVar = history.bestHeaderOpt.get.height + 1
    history.pruningProcessor.isHeadersChainSyncedVar = true
    history.applicableTry(section) shouldBe 'failure
    history.pruningProcessor.minimalFullBlockHeightVar = ErgoHistory.GenesisHeight

    // should not be able to apply if corresponding header is marked as invalid
    history.applicableTry(section) shouldBe 'success
    history.historyStorage.insert(Seq(history.validityKey(header.id) -> Array(0.toByte)), Seq.empty)
    history.isSemanticallyValid(header.id) shouldBe ModifierSemanticValidity.Invalid
    history.applicableTry(section) shouldBe 'failure
    history.historyStorage.insert(Seq(history.validityKey(header.id) -> Array(1.toByte)), Seq.empty)

    // should not be able to apply if already in history
    history.applicableTry(section) shouldBe 'success
    history.append(section).get
    history.applicableTry(section) shouldBe 'failure
  }

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  private def withUpdatedHeaderId[T <: BlockSection](section: T, newId: ModifierId): T = section match {
    case s: Extension => s.copy(headerId = newId).asInstanceOf[T]
    case s: BlockTransactions => s.copy(headerId = newId).asInstanceOf[T]
    case s: ADProofs => s.copy(headerId = newId).asInstanceOf[T]
  }

}
