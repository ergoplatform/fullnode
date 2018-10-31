package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, TokenId}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoState, StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.{BalancesSnapshot, ErgoWallet}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.{ErgoAddress, ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.{EvaluatedValue, LongConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.{SBoolean, SLong}

import scala.concurrent.Await
import scala.concurrent.duration._

trait WalletTestOps extends NodeViewBaseOps {

  def emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  def newAssetIdStub: TokenId = Digest32 @@ Array.emptyByteArray

  override def initSettings: ErgoSettings = {
    val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    settings.copy(walletSettings = settings.walletSettings.copy(scanningInterval = 15.millis))
  }

  def withFixture[T](test: WalletFixture => T): T = {
    new WalletFixture(settings, getCurrentView(_).vault).apply(test)
  }

  def wallet(implicit w: WalletFixture): ErgoWallet = w.wallet

  def getTrackedAddresses(implicit w: WalletFixture): Seq[ErgoAddress] =
    Await.result(w.wallet.trackedAddresses(), awaitDuration)

  def getConfirmedBalances(implicit w: WalletFixture): BalancesSnapshot =
    Await.result(w.wallet.confirmedBalances(), awaitDuration)

  def getBalancesWithUnconfirmed(implicit w: WalletFixture): BalancesSnapshot =
    Await.result(w.wallet.balancesWithUnconfirmed(), awaitDuration)

  def scanningInterval(implicit ctx: Ctx): Long = ctx.settings.walletSettings.scanningInterval.toMillis

  def scanTime(block: ErgoFullBlock)(implicit ctx: Ctx): Long = {
    val boxes = block.transactions.flatMap(_.outputs)
    val tokens = boxes.flatMap(_.additionalTokens)
    scanTime(boxes.size, tokens.size)
  }

  def scanTime(boxCount: Int, tokenCount: Int)(implicit ctx: Ctx): Long = {
    boxCount * scanningInterval + tokenCount * scanningInterval * 2 + 1000
  }

  def offchainScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

  def balanceAmount(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum

  def boxesAvailable(block: ErgoFullBlock, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    block.transactions.flatMap(boxesAvailable(_, script))
  }

  def boxesAvailable(tx: ErgoTransaction, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    tx.outputs.filter(_.proposition == script)
  }

  def assetAmount(boxes: Seq[ErgoBoxCandidate]): Map[ModifierId, Long] = {
    assetsByTokenId(boxes).map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
  }

  def toAssetMap(assetSeq: Seq[(TokenId, Long)]): Map[ModifierId, Long] = {
    assetSeq
      .map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
      .toMap
  }

  def assetsByTokenId(boxes: Seq[ErgoBoxCandidate]): Map[TokenId, Long] = {
    boxes
      .flatMap { _.additionalTokens }
      .groupBy { case (tokenId, _) => tokenId }
      .map { case (id, pairs) => id -> pairs.map(_._2).sum }
  }

  def getUtxoState(implicit ctx: Ctx): UtxoState = getCurrentState.asInstanceOf[UtxoState]

  def getHeightOf(state: ErgoState[_])(implicit ctx: Ctx): Option[Int] = {
    getHistory.heightOf(scorex.core.versionToId(state.version))
  }

  def makeGenesisBlock(script: Value[SBoolean.type], assets: Seq[(TokenId, Long)] = Seq.empty)
                      (implicit ctx: Ctx): ErgoFullBlock = {
    makeNextBlock(getUtxoState, Seq(makeGenesisTx(script, assets)))
  }

  def makeGenesisTx(script: Value[SBoolean.type], assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    //ErgoMiner.createCoinbase(Some(genesisEmissionBox), 0, Seq.empty, script, emission)
    val emissionBox = genesisEmissionBox
    val height = 0
    val emissionAmount = settings.emission.emissionAtHeight(height)
    val newEmissionAmount = emissionBox.value - emissionAmount
    val emissionRegs = Map[NonMandatoryRegisterId, EvaluatedValue[SLong.type]](R4 -> LongConstant(height))
    val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))
    val newEmissionBox = new ErgoBoxCandidate(newEmissionAmount, emissionBox.proposition, height, Seq.empty, emissionRegs)
    val minerBox = new ErgoBoxCandidate(emissionAmount, script, height, replaceNewAssetStub(assets, inputs), Map.empty)
    ErgoTransaction(inputs, IndexedSeq(newEmissionBox, minerBox))
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     addressToSpend: ErgoAddress,
                     balanceToReturn: Long = 0,
                     assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    makeTx(boxesToSpend, emptyProverResult, balanceToReturn, addressToSpend.script, assets)
  }

  def makeTx(boxesToSpend: Seq[ErgoBox],
             proofToSpend: ProverResult,
             balanceToReturn: Long,
             scriptToReturn: Value[SBoolean.type],
             assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val height = 0
    val inputs = boxesToSpend.map(box => Input(box.id, proofToSpend))
    val balanceToSpend = boxesToSpend.map(_.value).sum - balanceToReturn
    def creatingCandidate = new ErgoBoxCandidate(balanceToReturn, scriptToReturn, height, replaceNewAssetStub(assets, inputs))
    val spendingOutput = if (balanceToSpend > 0) Some(new ErgoBoxCandidate(balanceToSpend, TrueLeaf, height)) else None
    val creatingOutput = if (balanceToReturn > 0) Some(creatingCandidate) else None
    ErgoTransaction(inputs.toIndexedSeq, spendingOutput.toIndexedSeq ++ creatingOutput.toIndexedSeq)
  }

  private def replaceNewAssetStub(assets: Seq[(TokenId, Long)], inputs: Seq[Input]): Seq[(TokenId, Long)] = {
    def isNewAsset(tokenId: TokenId, value: Long): Boolean =  java.util.Arrays.equals(tokenId, newAssetIdStub)
    val (newAsset, spentAssets) = assets.partition((isNewAsset _).tupled)
    newAsset.map(Digest32 @@ inputs.head.boxId -> _._2) ++ spentAssets
  }

  def randomNewAsset: Seq[(TokenId, Long)] = Seq(newAssetIdStub -> assetGen.sample.value._2)
  def assetsWithRandom(boxes: Seq[ErgoBox]): Seq[(TokenId, Long)] = randomNewAsset ++ assetsByTokenId(boxes)
  def badAssets: Seq[(TokenId, Long)] = additionalTokensGen.sample.value
}
