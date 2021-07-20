package org.ergoplatform.utils.generators

import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.nodeView.wallet.scanning._
import org.ergoplatform.settings.Constants
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.utils.Generators
import org.scalacheck.Gen
import sigmastate.Values.ByteArrayConstant

import scala.collection.mutable

trait WalletGenerators extends ErgoTransactionGenerators with Generators {

  override def trackedBoxGen: Gen[TrackedBox] = {
    Gen.oneOf(
      unspentOffchainBoxGen,
      unspentOnchainBoxGen,
      spentOffchainBoxGen,
      spentPartiallyOffchainBoxGen,
      spentOnchainBoxGen
    )
  }

  def unspentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      appStatuses <- appStatusesGen
    } yield TrackedBox(tx, outIndex, None, ergoBox, appStatuses)
  }

  def unspentOnchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      appStatuses <- appStatusesGen
    } yield TrackedBox(tx, outIndex, Some(height), ergoBox, appStatuses)
  }

  def spentOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      ergoBox <- Gen.oneOf(boxes)
      appStatuses <- appStatusesGen
    } yield TrackedBox(tx.id, outIndex, None, Some(spendingTx.id), None, ergoBox, appStatuses)
  }

  def spentPartiallyOffchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      ergoBox <- Gen.oneOf(boxes)
      appStatuses <- appStatusesGen
    } yield TrackedBox(tx.id, outIndex, Some(height), Some(spendingTx.id), None, ergoBox, appStatuses)
  }

  def spentOnchainBoxGen: Gen[TrackedBox] = {
    for {
      (boxes, tx) <- validErgoTransactionGen
      (_, spendingTx) <- validErgoTransactionGen
      outIndex <- outIndexGen(tx)
      height <- heightGen()
      spendingHeight <- heightGen(height)
      ergoBox <- Gen.oneOf(boxes)
      appStatuses <- appStatusesGen
    } yield TrackedBox(
      tx.id, outIndex, Some(height), Some(spendingTx.id), Some(spendingHeight), ergoBox, appStatuses)
  }

  def paymentRequestGen: Gen[PaymentRequest] = {
    for {
      value <- Gen.choose(1L, 100000L)
      assets <- additionalTokensGen
      registers <- additionalRegistersGen
    } yield PaymentRequest(Pay2SAddress(Constants.FalseLeaf), value, assets, registers)
  }

  def assetIssueRequestGen: Gen[AssetIssueRequest] = {
    for {
      amount <- Gen.choose(1L, 100000L)
      name <- Gen.alphaUpperStr
      description <- Gen.alphaLowerStr
      decimals <- Gen.choose(4, 16)
    } yield AssetIssueRequest(Pay2SAddress(Constants.FalseLeaf), None, amount, name, description, decimals)
  }

  def registrySummaryGen: Gen[WalletDigest] = {
    for {
      height <- Gen.posNum[Int]
      amount <- Gen.choose(1L, 100000L)
      balances <- additionalTokensGen
    } yield {
      val encodedBalances = balances.map { case (x1, x2) => encodedTokenId(x1) -> x2 }
      WalletDigest(height, amount, encodedBalances)
    }
  }

  def registerIdGen: Gen[ErgoBox.RegisterId] = Gen.oneOf(ErgoBox.allRegisters)

  def containsScanningPredicateGen: Gen[ContainsScanningPredicate] = for {
    regId <- registerIdGen
    bs <- nonEmptyBytesGen
  } yield ContainsScanningPredicate(regId, ByteArrayConstant(bs))

  def equalsScanningPredicateGen: Gen[EqualsScanningPredicate] = for {
    regId <- registerIdGen
    bs <- nonEmptyBytesGen
  } yield EqualsScanningPredicate(regId, ByteArrayConstant(bs))

  def containsAssetPredicateGen: Gen[ContainsAssetPredicate] = for {
    asset <- assetGen
  } yield ContainsAssetPredicate(asset._1)

  def orScanningPredicateGen: Gen[OrScanningPredicate] = for {
    args <- Gen.nonEmptyListOf(Gen.oneOf(containsScanningPredicateGen, equalsScanningPredicateGen,
      containsAssetPredicateGen))
  } yield OrScanningPredicate(args: _*)

  def andScanningPredicateGen: Gen[AndScanningPredicate] = for {
    args <- Gen.nonEmptyListOf(Gen.oneOf(containsScanningPredicateGen, equalsScanningPredicateGen,
      containsAssetPredicateGen, orScanningPredicateGen))
  } yield AndScanningPredicate(args: _*)

  def scanningPredicateGen: Gen[ScanningPredicate] = for {
    predicate <- Gen.oneOf(andScanningPredicateGen, orScanningPredicateGen)
  } yield predicate

  def externalScanReqGen: Gen[ScanRequest] = for {
    appName <- Gen.alphaNumStr
    pred <- scanningPredicateGen
  } yield ScanRequest(appName, pred, Some(ScanWalletInteraction.Off), Some(true))

  def externalAppGen: Gen[Scan] = for {
    scanId <- Gen.posNum[Short]
    req <- externalScanReqGen
  } yield req.toScan(ScanId @@ scanId).get

  private def outIndexGen(tx: ErgoTransaction) = Gen.choose(0: Short, tx.outputCandidates.length.toShort)

  private def heightGen(min: Int = 0) = Gen.choose(min + 1, Integer.MAX_VALUE / 2 + min)

}
