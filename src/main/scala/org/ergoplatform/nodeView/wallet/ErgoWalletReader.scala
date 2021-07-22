package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.{ErgoBox, P2PKAddress}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.scanning.ScanRequest
import org.ergoplatform.nodeView.wallet.requests.{BoxesRequest, ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.wallet.boxes.ChainStatus
import org.ergoplatform.wallet.boxes.ChainStatus.{OffChain, OnChain}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import scorex.core.transaction.wallet.VaultReader
import scorex.util.ModifierId
import sigmastate.Values.SigmaBoolean

import scala.concurrent.Future
import scala.util.Try

trait ErgoWalletReader extends VaultReader {

  val walletActor: ActorRef

  private implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  /** Returns the Future generated mnemonic phrase.
    * @param pass   storage encription password
    * @param mnemonicPassOpt  mnemonic encription password
    * @return  menmonic phrase for the new wallet
    */
  def initWallet(pass: String, mnemonicPassOpt: Option[String]): Future[Try[String]] =
    (walletActor ? InitWallet(pass, mnemonicPassOpt)).mapTo[Try[String]]

  def restoreWallet(encryptionPass: String, mnemonic: String,
                    mnemonicPassOpt: Option[String] = None): Future[Try[Unit]] =
    (walletActor ? RestoreWallet(mnemonic, mnemonicPassOpt, encryptionPass)).mapTo[Try[Unit]]

  def unlockWallet(pass: String): Future[Try[Unit]] =
    (walletActor ? UnlockWallet(pass)).mapTo[Try[Unit]]

  def lockWallet(): Unit = walletActor ! LockWallet

  def rescanWallet(): Future[Try[Unit]] = (walletActor ? RescanWallet).mapTo[Try[Unit]]

  def getWalletStatus: Future[WalletStatus] =
    (walletActor ? GetWalletStatus).mapTo[WalletStatus]

  def checkSeed(mnemonic: String, mnemonicPassOpt: Option[String] = None): Future[Boolean] = (walletActor ? CheckSeed(mnemonic, mnemonicPassOpt)).mapTo[Boolean]

  def deriveKey(path: String): Future[Try[P2PKAddress]] =
    (walletActor ? DeriveKey(path)).mapTo[Try[P2PKAddress]]

  def deriveNextKey: Future[DeriveNextKeyResult] =
    (walletActor ? DeriveNextKey).mapTo[DeriveNextKeyResult]

  def balances(chainStatus: ChainStatus): Future[WalletDigest] =
    (walletActor ? ReadBalances(chainStatus)).mapTo[WalletDigest]

  def confirmedBalances: Future[WalletDigest] = balances(OnChain)

  def balancesWithUnconfirmed: Future[WalletDigest] = balances(OffChain)

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] =
    (walletActor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]

  def walletBoxes(unspentOnly: Boolean, considerUnconfirmed: Boolean): Future[Seq[WalletBox]] =
    (walletActor ? GetWalletBoxes(unspentOnly, considerUnconfirmed)).mapTo[Seq[WalletBox]]

  def appBoxes(scanId: ScanId,
               unspentOnly: Boolean = false,
               considerUnconfirmed: Boolean = false): Future[Seq[WalletBox]] =
    (walletActor ? GetScanBoxes(scanId, unspentOnly, considerUnconfirmed)).mapTo[Seq[WalletBox]]

  def updateChangeAddress(address: P2PKAddress): Unit =
    walletActor ! UpdateChangeAddress(address)

  def transactions(filteringOpts: Option[WalletFiltering]): Future[Seq[AugWalletTransaction]] =
    (walletActor ? GetTransactions(filteringOpts)).mapTo[Seq[AugWalletTransaction]]

  def transactionById(id: ModifierId): Future[Option[AugWalletTransaction]] =
    (walletActor ? GetTransaction(id)).mapTo[Option[AugWalletTransaction]]

  def generateTransaction(requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String] = Seq.empty,
                          dataInputsRaw: Seq[String] = Seq.empty): Future[Try[ErgoTransaction]] =
    (walletActor ? GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign = true)).mapTo[Try[ErgoTransaction]]

  def generateCommitmentsFor(unsignedErgoTransaction: UnsignedErgoTransaction,
                             externalSecretsOpt: Option[Seq[ExternalSecret]],
                             boxesToSpend: Option[Seq[ErgoBox]],
                             dataBoxes: Option[Seq[ErgoBox]]): Future[GenerateCommitmentsResponse] =
    (walletActor ? GenerateCommitmentsFor(unsignedErgoTransaction, externalSecretsOpt, boxesToSpend, dataBoxes))
      .mapTo[GenerateCommitmentsResponse]


  def generateUnsignedTransaction(requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String] = Seq.empty,
                          dataInputsRaw: Seq[String] = Seq.empty): Future[Try[UnsignedErgoTransaction]] =
    (walletActor ? GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign = false)).mapTo[Try[UnsignedErgoTransaction]]


  def signTransaction(tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpend: Option[Seq[ErgoBox]],
                      dataBoxes: Option[Seq[ErgoBox]]): Future[Try[ErgoTransaction]] =
    (walletActor ? SignTransaction(tx, secrets, hints, boxesToSpend, dataBoxes)).mapTo[Try[ErgoTransaction]]

  def extractHints(tx: ErgoTransaction,
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean],
                   boxesToSpend: Option[Seq[ErgoBox]],
                   dataBoxes: Option[Seq[ErgoBox]]): Future[ExtractHintsResult] =
    (walletActor ? ExtractHints(tx, real, simulated, boxesToSpend, dataBoxes)).mapTo[ExtractHintsResult]

  def addScan(appRequest: ScanRequest): Future[AddScanResponse] =
    (walletActor ? AddScan(appRequest)).mapTo[AddScanResponse]

  def removeScan(scanId: ScanId): Future[RemoveScanResponse] =
    (walletActor ? RemoveScan(scanId)).mapTo[RemoveScanResponse]

  def readScans(): Future[ReadScansResponse] =
    (walletActor ? ReadScans).mapTo[ReadScansResponse]

  def stopTracking(scanId: ScanId, boxId: BoxId): Future[StopTrackingResponse] =
    (walletActor ? StopTracking(scanId, boxId)).mapTo[StopTrackingResponse]

  def addBox(box: ErgoBox, scanIds: Set[ScanId]): Future[AddBoxResponse] =
    (walletActor ? AddBox(box, scanIds)).mapTo[AddBoxResponse]

  def collectBoxes(request: BoxesRequest): Future[ReqBoxesResponse] =
    (walletActor ? CollectWalletBoxes(request.targetBalance, request.targetAssets)).mapTo[ReqBoxesResponse]

  def transactionsByScanId(scanId: ScanId): Future[ScanRelatedTxsResponse] =
    (walletActor ? GetScanTransactions(scanId)).mapTo[ScanRelatedTxsResponse]

}
