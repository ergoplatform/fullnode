package org.ergoplatform.mining

import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{LaunchParameters, MonetarySettings}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.util.Random

class ErgoMinerPropSpec extends ErgoPropertyTest {

  val delta: Int = settings.emission.settings.minerRewardDelay

  private def expectedRewardOutputScriptBytes(pk: ProveDlog): Array[Byte] =
    ErgoState.rewardOutputScript(delta, pk).bytes

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("collect reward from emission box only") {
    val us = createUtxoState()._1
    us.emissionBoxOpt should not be None
    val expectedReward = us.constants.emission.emissionAtHeight(us.stateContext.currentHeight)

    val incorrectTxs = ErgoMiner.collectEmission(us, proveDlogGen.sample.get, us.constants.emission).toSeq
    val txs = ErgoMiner.collectEmission(us, defaultMinerPk, us.constants.emission).toSeq

    txs.size shouldBe 1
    val emissionTx = txs.head
    emissionTx.outputs.length shouldBe 2
    emissionTx.outputs.last.value shouldBe expectedReward
    emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

    us.applyModifier(validFullBlock(None, us, incorrectTxs)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, txs)) shouldBe 'success
  }

  property("collect reward from transaction fees only") {
    val bh = boxesHolderGen.sample.get
    val us = createUtxoState(bh)
    val height = us.stateContext.currentHeight
    val blockTx = validTransactionFromBoxes(bh.boxes.take(10).values.toIndexedSeq, outputsProposition = feeProp)

    val txs = ErgoMiner.collectFees(height, Seq(blockTx), defaultMinerPk, settings.emission).toSeq
    val incorrect = ErgoMiner.collectFees(height, Seq(blockTx), proveDlogGen.sample.get, settings.emission).toSeq
    txs.length shouldBe 1
    val feeTx = txs.head
    feeTx.outputs.length shouldBe 1
    feeTx.outputs.head.value shouldBe txs.flatMap(_.outputs).map(_.value).sum
    feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

    us.applyModifier(validFullBlock(None, us, blockTx +: incorrect)) shouldBe 'failure
    us.applyModifier(validFullBlock(None, us, blockTx +: txs)) shouldBe 'success
  }


  property("filter out double spend txs") {
    val cost = 1L
    val tx = validErgoTransactionGen.sample.get._2 -> cost
    ErgoMiner.fixTxsConflicts(Seq(tx, tx, tx)) should have length 1

    val inputs = validErgoTransactionGenTemplate(0, -1, 100).sample.get._1
    val (l, r) = inputs.splitAt(50)
    val tx_1 = validTransactionFromBoxes(l) -> cost
    val tx_2 = validTransactionFromBoxes(r :+ l.last) -> cost

    ErgoMiner.fixTxsConflicts(Seq(tx_1, tx_2, tx)) should contain theSameElementsAs Seq(tx_1, tx)
    ErgoMiner.fixTxsConflicts(Seq(tx_2, tx_1, tx)) should contain theSameElementsAs Seq(tx_2, tx)
  }

  property("should only collect valid transactions") {
    def checkCollectTxs(maxCost: Long, maxSize: Int, withTokens: Boolean): Unit = {

      val bh = boxesHolderGen.sample.get
      val rnd: Random = new Random
      val us = createUtxoState(bh)
      val usClone = createUtxoState(bh)
      val feeProposition = ErgoState.feeProposition(delta)
      val inputs = bh.boxes.values.toIndexedSeq.takeRight(100)
      val txsWithFees = inputs.map(i => validTransactionFromBoxes(IndexedSeq(i), rnd, issueNew = withTokens, feeProposition))
      val head = txsWithFees.head

      usClone.applyModifier(validFullBlock(None, us, bh, rnd)).get
      val upcomingContext = usClone.stateContext
      upcomingContext.currentHeight shouldBe (us.stateContext.currentHeight + 1)

      val fromSmallMempool = ErgoMiner.collectTxs(defaultMinerPk, maxCost, maxSize, us, upcomingContext, Seq(head), Seq())
      fromSmallMempool.size shouldBe 2
      fromSmallMempool.contains(head) shouldBe true

      val fromBigMempool = ErgoMiner.collectTxs(defaultMinerPk, maxCost, maxSize, us, upcomingContext, txsWithFees, Seq())

      val newBoxes = fromBigMempool.flatMap(_.outputs)
      val costs = fromBigMempool.map { tx =>
        us.validateWithCost(tx).getOrElse {
          val boxesToSpend = tx.inputs.map(i => newBoxes.find(b => b.id sameElements i.boxId).get)
          tx.statefulValidity(boxesToSpend, upcomingContext).get
        }
      }

      fromBigMempool.length should be > 1
      fromBigMempool.map(_.size).sum should be < maxSize
      costs.sum should be < maxCost
      fromBigMempool.size should be < txsWithFees.size
    }

    // transactions reach computation cost block limit
    checkCollectTxs(100000L, Int.MaxValue, withTokens = false)

    // transactions reach block size limit
    checkCollectTxs(Long.MaxValue, 4096, withTokens = false)

    // too many tokens in fees
    checkCollectTxs(Long.MaxValue, Int.MaxValue, withTokens = true)

  }


  property("should not be able to spend recent fee boxes") {

    val delta = 1
    val feeProposition = ErgoState.feeProposition(delta)

    val localSettings = settings.copy(
      chainSettings = settings.chainSettings.copy(
        monetary = settings.chainSettings.monetary.copy(minerRewardDelay = delta)
      )
    )

    val emissionRules = new EmissionRules(localSettings.chainSettings.monetary)

    val bh = boxesHolderGen.sample.get
    var us = createUtxoStateWithCustomSettings(bh, localSettings)
    val height = ErgoHistory.EmptyHistoryHeight

    val blockTx = validTransactionFromBoxes(bh.boxes.take(5).values.toIndexedSeq, outputsProposition = feeProposition)
    val txs = ErgoMiner.collectFees(height, Seq(blockTx), defaultMinerPk, emissionRules).toSeq
    val block = validFullBlock(None, us, blockTx +: txs)

    us = us.applyModifier(block).get

    val blockTx2 = validTransactionFromBoxes(
      bh.boxes.slice(10, 20).values.toIndexedSeq, outputsProposition = feeProposition)
    val block2 = validFullBlock(Some(block.header), us, IndexedSeq(blockTx2))

    val earlySpendingTx = validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val invalidBlock2 = validFullBlock(Some(block.header), us, IndexedSeq(earlySpendingTx, blockTx2))

    us.applyModifier(invalidBlock2) shouldBe 'failure

    us = us.applyModifier(block2).get

    val earlySpendingTx2 = validTransactionFromBoxes(txs.head.outputs, stateCtxOpt = Some(us.stateContext))

    val blockTx3 = validTransactionFromBoxes(
      bh.boxes.slice(20, 30).values.toIndexedSeq, outputsProposition = feeProposition)
    val block3 = validFullBlock(Some(block2.header), us, IndexedSeq(earlySpendingTx2, blockTx3))

    us.applyModifier(block3) shouldBe 'success
  }

  property("collect reward from both emission box and fees") {
    val (us, _) = createUtxoState()
    us.emissionBoxOpt should not be None
    val expectedReward = us.constants.emission.emissionAtHeight(us.stateContext.currentHeight)

    forAll(Gen.nonEmptyListOf(validErgoTransactionGenTemplate(0, propositionGen = feeProp))) { btxs =>
      val blockTxs = btxs.map(_._2)
      val height = ErgoHistory.EmptyHistoryHeight
      val txs = ErgoMiner.collectRewards(us.emissionBoxOpt, height, blockTxs, defaultMinerPk, settings.emission)
      txs.length shouldBe 2

      val emissionTx = txs.head
      emissionTx.outputs.length shouldBe 2
      emissionTx.outputs.last.value shouldBe expectedReward
      emissionTx.outputs.last.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)

      val feeTx = txs.last
      feeTx.outputs.length shouldBe 1
      feeTx.outputs.head.value shouldBe blockTxs.flatMap(_.outputs).map(_.value).sum
      feeTx.outputs.head.propositionBytes shouldEqual expectedRewardOutputScriptBytes(defaultMinerPk)
    }
  }

}
