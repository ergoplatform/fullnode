package org.ergoplatform.nodeView.wallet

import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.utils.{ErgoPropertyTest, WalletTestOps}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import WalletScanLogic.{extractWalletOutputs, scanBlockTransactions}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.ScanId
import org.scalacheck.Gen
import sigmastate.Values.{ByteArrayConstant, ErgoTree, FalseLeaf, TrueLeaf}

import scala.util.Random

class WalletScanLogicSpec extends ErgoPropertyTest with DBSpec with WalletTestOps {

  private case class TrackedTransaction(tx: ErgoTransaction,
                                        payments: List[ErgoTree],
                                        paymentValues: List[Int],
                                        appPayments: List[ErgoTree],
                                        appPaymentValues: List[Int],
                                        miningRewards: List[ErgoTree],
                                        miningRewardValues: List[Int]) {
    def scriptsCount: Int = payments.size + appPayments.size + miningRewards.size

    def valuesSum: Long = (paymentValues ++ appPaymentValues ++ miningRewardValues).sum
  }

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)
  private val prover = defaultProver
  private val monetarySettings = initSettings.chainSettings.monetary.copy(minerRewardDelay = 720)
  private val s = initSettings.copy(chainSettings = initSettings.chainSettings.copy(monetary = monetarySettings))

  private val trueProp = org.ergoplatform.settings.Constants.TrueLeaf
  private val scanningPredicate = EqualsScanningPredicate(ErgoBox.ScriptRegId, ByteArrayConstant(trueProp.bytes))
  private val appReq = ScanRequest("True detector", scanningPredicate, Some(ScanWalletInteraction.Off), None)
  private val scanId: ScanId = ScanId @@ 50.toShort

  private val pubkeys = prover.hdPubKeys
  private val miningScripts = WalletCache.miningScripts(pubkeys, s)

  private def paymentsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.oneOf(pubkeys.map(_.key.toSigmaProp: ErgoTree)))

  private def miningRewardsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.oneOf(miningScripts))

  private def nonTrackablePaymentsGen: Gen[List[ErgoTree]] = Gen.nonEmptyListOf(Gen.const(FalseLeaf.toSigmaProp))

  private def appPaymentsGen: Gen[List[ErgoTree]] = Gen.listOf(Gen.const(trueProp))

  private def walletVarsGen: Gen[WalletVars] = {
    for {
      // To check that scanning works with locked wallet, we randomly choose whether the prover is set or not
      proverSet <- Gen.oneOf(true, false)
    } yield {
      if (proverSet) {
        WalletVars(Some(prover), Seq(appReq.toScan(scanId).get), None)(s)
      } else {
        val cache = WalletCache(pubkeys, s)
        WalletVars(None, Seq(appReq.toScan(scanId).get), Some(cache))(s)
      }
    }
  }

  private def trackedTransactionGen: Gen[TrackedTransaction] = {
    for {
      payments <- paymentsGen
      appPayments <- appPaymentsGen
      miningRewards <- miningRewardsGen
      nonTrackablePayments <- nonTrackablePaymentsGen
    } yield {
      def valueGen(): Int = Random.nextInt(1000) + 100

      val appPaymentValues = appPayments.map(_ => valueGen())
      val paymentValues = payments.map(_ => valueGen())
      val miningRewardValues = miningRewards.map(_ => valueGen())
      val nonTrackableValues = nonTrackablePayments.map(_ => valueGen())
      val outs = (payments ++ appPayments ++ miningRewards ++ nonTrackablePayments)
        .zip(paymentValues ++ appPaymentValues ++ miningRewardValues ++ nonTrackableValues)
        .map { case (script, vl) => new ErgoBoxCandidate(vl, script, creationHeight = 1) }
        .toIndexedSeq

      val tx = new ErgoTransaction(fakeInputs, IndexedSeq.empty, outs)

      TrackedTransaction(tx, payments, paymentValues, appPayments, appPaymentValues, miningRewards, miningRewardValues)
    }
  }

  property("extractWalletOutputs properly extracts") {
    val height = Random.nextInt(200) - 100
    val inclusionHeightOpt = if (height <= 0) None else Some(height)

    forAll(trackedTransactionGen, walletVarsGen) { case (trackedTransaction, walletVars) =>
      val foundBoxes = extractWalletOutputs(trackedTransaction.tx, inclusionHeightOpt, walletVars)
      foundBoxes.length shouldBe trackedTransaction.scriptsCount
      foundBoxes.map(_.inclusionHeightOpt).forall(_ == inclusionHeightOpt) shouldBe true
      foundBoxes.map(_.value).sum shouldBe trackedTransaction.valuesSum
      foundBoxes.forall(tb => if (trackedTransaction.payments.contains(tb.box.ergoTree)) {
        tb.scans == Set(Constants.PaymentsScanId)
      } else if (trackedTransaction.miningRewards.contains(tb.box.ergoTree)) {
        tb.scans == Set(Constants.MiningScanId)
      } else {
        tb.scans == Set(scanId)
      }) shouldBe true
    }
  }

  property("scanBlockTransactions") {
    withVersionedStore(10) { store =>
      val walletVars = walletVarsGen.sample.get
      val emptyReg = new WalletRegistry(store)(settings.walletSettings)
      val emptyOff = OffChainRegistry.empty
      val blockId = modIdGen.sample.get

      val height0 = 5
      //simplest case - we're scanning an empty block
      val (r0, o0, f0) =
        scanBlockTransactions(emptyReg, emptyOff, emptyStateContext, walletVars, height0, blockId, Seq.empty, None).get
      val r0digest = r0.fetchDigest()
      r0digest.walletBalance shouldBe 0
      r0digest.walletAssetBalances.size shouldBe 0
      r0digest.height shouldBe height0

      val o0digest = o0.digest
      o0digest.walletBalance shouldBe 0
      o0digest.walletAssetBalances.size shouldBe 0
      o0digest.height shouldBe height0

      var registry = r0
      var off = o0

      forAll(trackedTransactionGen) { trackedTransaction =>
        //applying one transaction creating boxes
        val creatingTx = trackedTransaction.tx
        val txs = Seq(creatingTx)
        val height1 = 5

        val regDigestBefore = registry.fetchDigest().walletBalance
        val offDigestBefore = off.digest.walletBalance

        val (r1, o1, f1) =
          scanBlockTransactions(registry, off, emptyStateContext, walletVars, height1, blockId, txs, Some(f0)).get
        val r1digest = r1.fetchDigest()
        r1digest.walletBalance shouldBe (regDigestBefore + trackedTransaction.paymentValues.sum)
        r1digest.walletAssetBalances.size shouldBe 0
        r1digest.height shouldBe height1

        val o1digest = o1.digest
        o1digest.walletBalance shouldBe (offDigestBefore + trackedTransaction.paymentValues.sum)
        o1digest.walletAssetBalances.size shouldBe 0
        o1digest.height shouldBe height1

        registry = r1
        off = o1

        //applying a transaction spending outputs of previous transaction and creating new one with the same outputs
        val tx = trackedTransaction.tx
        val inputs = tx.outputs.map(_.id).map(id => Input(id, emptyProverResult))
        val spendingTx = ErgoTransaction(inputs, IndexedSeq.empty, tx.outputCandidates)

        val (r2, o2, f2) =
          scanBlockTransactions(registry, off, emptyStateContext, walletVars, height1 + 1, blockId, Seq(spendingTx), Some(f1)).get

        val r2digest = r2.fetchDigest()
        r2digest.walletBalance shouldBe (regDigestBefore + trackedTransaction.paymentValues.sum)
        r2digest.walletAssetBalances.size shouldBe 0
        r2digest.height shouldBe height1 + 1

        val o2digest = o2.digest
        o2digest.walletBalance shouldBe (offDigestBefore + trackedTransaction.paymentValues.sum)
        o2digest.walletAssetBalances.size shouldBe 0
        o2digest.height shouldBe height1 + 1

        registry = r2
        off = o2

        //applying a transaction spending outputs of the previous transaction
        val inputs2 = spendingTx.outputs.map(_.id).map(id => Input(id, emptyProverResult))
        val outputs2 = IndexedSeq(new ErgoBoxCandidate(spendingTx.outputs.map(_.value).sum, FalseLeaf.toSigmaProp, height1))
        val spendingTx2 = new ErgoTransaction(inputs2, IndexedSeq.empty, outputs2)

        val (r3, o3, f3) =
          scanBlockTransactions(registry, off, emptyStateContext, walletVars, height1 + 2, blockId, Seq(spendingTx2), Some(f2)).get

        val r3digest = r3.fetchDigest()
        r3digest.walletBalance shouldBe regDigestBefore
        r3digest.walletAssetBalances.size shouldBe 0
        r3digest.height shouldBe height1 + 2

        val o3digest = o3.digest
        o3digest.walletBalance shouldBe offDigestBefore
        o3digest.walletAssetBalances.size shouldBe 0
        o3digest.height shouldBe height1 + 2

        registry = r3
        off = o3

        //applying all the three previous transactions
        val threeTxs = Seq(creatingTx, spendingTx, spendingTx2)

        val (r4, o4, _) =
          scanBlockTransactions(registry, off, emptyStateContext, walletVars, height1 + 3, blockId, threeTxs, Some(f3)).get

        val r4digest = r4.fetchDigest()
        r4digest.walletBalance shouldBe regDigestBefore
        r4digest.walletAssetBalances.size shouldBe 0
        r4digest.height shouldBe height1 + 3
        r4.walletUnspentBoxes() shouldBe Seq.empty

        val o4digest = o4.digest
        o4digest.walletBalance shouldBe offDigestBefore
        o4digest.walletAssetBalances.size shouldBe 0
        o4digest.height shouldBe height1 + 3

        registry = r4
        off = o4
      }
    }
  }

  property("external scan prioritized over payments one if walletInteraction = off, otherwise shared") {
    val intFlagGen = Gen.oneOf(ScanWalletInteraction.Off, ScanWalletInteraction.Shared, ScanWalletInteraction.Forced)
    forAll(intFlagGen) { intFlag =>
      val pk = pubkeys.head.key.toSigmaProp: ErgoTree
      val outs = IndexedSeq(new ErgoBoxCandidate(1000, pk, creationHeight = 1))
      val tx = new ErgoTransaction(fakeInputs, IndexedSeq.empty, outs)

      val cache = WalletCache(pubkeys, s)
      val paymentPredicate = EqualsScanningPredicate(ErgoBox.ScriptRegId, ByteArrayConstant(pk.bytes))
      val paymentScanReq = ScanRequest("Payment scan", paymentPredicate, Some(intFlag), Some(true))
      val walletVars = WalletVars(None, Seq(paymentScanReq.toScan(scanId).get), Some(cache))(s)

      val boxes = extractWalletOutputs(tx, Some(1), walletVars)

      if (intFlag == ScanWalletInteraction.Shared || intFlag == ScanWalletInteraction.Forced) {
        boxes.size shouldBe 1
        boxes.head.scans.size shouldBe 2
        boxes.head.scans shouldBe Set(scanId, Constants.PaymentsScanId)
      } else {
        boxes.size shouldBe 1
        boxes.head.scans.size shouldBe 1
        boxes.head.scans.head shouldBe scanId
      }
    }
  }

  property("scan with forced flag is sharing boxes with the p2k-wallet") {
    val trueProp = TrueLeaf.toSigmaProp.treeWithSegregation: ErgoTree
    val outs = IndexedSeq(new ErgoBoxCandidate(1000, trueProp, creationHeight = 1))
    val tx = new ErgoTransaction(fakeInputs, IndexedSeq.empty, outs)

    val cache = WalletCache(pubkeys, s)
    val paymentPredicate = EqualsScanningPredicate(ErgoBox.ScriptRegId, ByteArrayConstant(trueProp.bytes))
    val paymentScanReq = ScanRequest("Payment scan", paymentPredicate, Some(ScanWalletInteraction.Forced), Some(false))
    val walletVars = WalletVars(None, Seq(paymentScanReq.toScan(scanId).get), Some(cache))(s)

    val boxes = extractWalletOutputs(tx, Some(1), walletVars)

    boxes.size shouldBe 1
    boxes.head.scans.size shouldBe 2
    boxes.head.scans shouldBe Set(scanId, Constants.PaymentsScanId)
  }

}
