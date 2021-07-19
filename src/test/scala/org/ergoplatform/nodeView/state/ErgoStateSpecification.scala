package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.{Args, ErgoSettings}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.core.bytesToVersion

import scala.util.Random

class ErgoStateSpecification extends ErgoPropertyTest {

  property("applyModifier() - double spending") {
    forAll(boxesHolderGen, Gen.choose(1: Byte, 2: Byte)) { case (bh, version) =>
      val us = createUtxoState(bh)
      val ds = createDigestState(bytesToVersion(Array.fill(32)(100: Byte)), us.rootHash)

      val validBlock = validFullBlock(None, us, bh)
      val dsTxs = validBlock.transactions ++ validBlock.transactions
      val changes = ErgoState.stateChanges(dsTxs)
      val toRemove = changes.toRemove.map(_.boxId)
      toRemove.count(boxId => java.util.Arrays.equals(toRemove.head, boxId)) shouldBe 2

      val dsRoot = BlockTransactions.transactionsRoot(dsTxs, version)
      val dsHeader = validBlock.header.copy(transactionsRoot = dsRoot)
      val bt = BlockTransactions(dsHeader.id, version, dsTxs)
      val doubleSpendBlock = ErgoFullBlock(dsHeader, bt, validBlock.extension, validBlock.adProofs)

      us.applyModifier(doubleSpendBlock) shouldBe 'failure
      us.applyModifier(validBlock) shouldBe 'success

      ds.applyModifier(doubleSpendBlock) shouldBe 'failure
      ds.applyModifier(validBlock) shouldBe 'success
    }
  }

  property("stateContext should be the same for Utxo and Digest states") {
    def requireEqualStateContexts(s1: ErgoStateContext, s2: ErgoStateContext, lastHeaders: Seq[Header]): Unit = {
      s1.currentHeight shouldBe lastHeaders.headOption.map(_.height).getOrElse(0)
      s1.currentHeight shouldBe s2.currentHeight
      s1.previousStateDigest shouldEqual s2.previousStateDigest
      s1.lastHeaders shouldEqual s2.lastHeaders
      s1.lastHeaders shouldEqual lastHeaders
      s1.currentParameters shouldEqual s2.currentParameters
      s1.votingData shouldEqual s2.votingData
      s1.genesisStateDigest shouldBe s2.genesisStateDigest
    }

    var (us, bh) = createUtxoState()
    var ds = createDigestState(us.version, us.rootHash)
    var lastBlocks: Seq[ErgoFullBlock] = Seq()
    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(lastBlocks.headOption, us, bh, new Random(seed))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block).get
      us = us.applyModifier(block).get
      lastBlocks = block +: lastBlocks
      requireEqualStateContexts(us.stateContext, ds.stateContext, lastBlocks.map(_.header))
    }
  }

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(Args.empty)
    val dir = createTempDir
    val rootHash = createUtxoState()._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings).rootHash
    rootHash shouldBe expectedRootHash
  }

  property("ErgoState.boxChanges() should generate operations in the same order") {
    var (us, bh) = createUtxoState()
    var parentOpt: Option[ErgoFullBlock] = None

    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(parentOpt, us, bh, new Random(seed))
      val block = blBh._1
      parentOpt = Some(block)
      bh = blBh._2
      us = us.applyModifier(block).get

      val changes1 = ErgoState.boxChanges(block.transactions)
      val changes2 = ErgoState.boxChanges(block.transactions)
      changes1._1 shouldBe changes2._1
      changes1._2 shouldBe changes2._2
    }
  }

  property("ErgoState.boxChanges() double spend attempt") {
    val (_, bh) = createUtxoState()
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new Random(seed))._1
      whenever(txs.lengthCompare(2) > 0) {
        // valid transaction should spend the only existing genesis box
        ErgoState.boxChanges(txs)._1.length shouldBe 1
        ErgoState.boxChanges(txs)._1.head shouldBe emissionBox.id

        // second transaction input should be an input created by the first transaction
        val inputToDoubleSpend = txs(1).inputs.head
        txs.head.outputs.find(b => java.util.Arrays.equals(b.id, inputToDoubleSpend.boxId)) should not be None
        val doubleSpendTx = txs.last.copy(inputs = inputToDoubleSpend +: txs.last.inputs.tail)
        val invalidTxs = txs.dropRight(1) :+ doubleSpendTx
        invalidTxs.length shouldBe txs.length
        invalidTxs.count(_.inputs.contains(inputToDoubleSpend)) shouldBe 2

        ErgoState.boxChanges(invalidTxs)._1.length shouldBe 2
      }
    }
  }

  property("ErgoState.stateChanges()") {
    val bh = createUtxoState()._2
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new Random(seed))._1
      whenever(txs.lengthCompare(1) > 0) {
        val changes = ErgoState.stateChanges(txs)
        val removals = changes.toRemove
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head.boxId shouldEqual emissionBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.toAppend
        // sum of coins in outputs should equal to genesis value
        insertions.map(_.box.value).sum shouldBe emissionBox.value

        // if output was spend and then created - it is in both toInsert and toRemove
        val changesRev = ErgoState.stateChanges(txs.reverse)
        val removalsRev = changesRev.toRemove
        val insertionsRev = changesRev.toAppend
        removalsRev.length should be > removals.length
        insertionsRev.length should be > insertions.length
      }
    }
  }

}
