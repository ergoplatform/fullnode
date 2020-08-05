package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.mining.ErgoMiner
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.TokensMap
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.bytesToId
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.ProverResult

import scala.concurrent.blocking

trait WalletTestOps extends NodeViewBaseOps {

  def newAssetIdStub: TokenId = Blake2b256.hash("new_asset")

  def withFixture[T](test: WalletFixture => T): T = {
    new WalletFixture(settings, getCurrentView(_).vault).apply(test)
  }

  def wallet(implicit w: WalletFixture): ErgoWallet = w.wallet

  def getPublicKeys(implicit w: WalletFixture): Seq[P2PKAddress] =
    await(w.wallet.publicKeys(0, Int.MaxValue))

  def getConfirmedBalances(implicit w: WalletFixture): RegistryIndex =
    await(w.wallet.confirmedBalances)

  def getBalancesWithUnconfirmed(implicit w: WalletFixture): RegistryIndex =
    await(w.wallet.balancesWithUnconfirmed)

  def scanningInterval(implicit ctx: Ctx): Long = ctx.settings.walletSettings.scanningInterval.toMillis

  def waitForScanning(block: ErgoFullBlock)(implicit ctx: Ctx): Unit = {
    blocking(Thread.sleep(scanTime(block)))
  }

  def scanTime(block: ErgoFullBlock)(implicit ctx: Ctx): Long = {
    val boxes = block.transactions.flatMap(_.outputs)
    val tokens = boxes.flatMap(_.additionalTokens.toArray)
    scanTime(boxes.size, tokens.size)
  }

  def scanTime(boxCount: Int, tokenCount: Int)(implicit ctx: Ctx): Long = {
    boxCount * scanningInterval + tokenCount * scanningInterval * 2 + 2500
  }

  def waitForOffchainScanning(tx: ErgoTransaction): Unit = {
    blocking(Thread.sleep(offchainScanTime(tx)))
  }

  def offchainScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

  def balanceAmount(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum

  def boxesAvailable(block: ErgoFullBlock, bytes: Array[Byte]): Seq[ErgoBox] = {
    block.transactions.flatMap(boxesAvailable(_, bytes))
  }

  def boxesAvailable(tx: ErgoTransaction, bytes: Array[Byte]): Seq[ErgoBox] = {
    tx.outputs.filter(_.propositionBytes.containsSlice(bytes))
  }

  def boxesAvailable(block: ErgoFullBlock, pk: ProveDlog): Seq[ErgoBox] = {
    block.transactions.flatMap(boxesAvailable(_, pk))
  }

  def boxesAvailable(tx: ErgoTransaction, pk: ProveDlog): Seq[ErgoBox] = {
    tx.outputs.filter(_.propositionBytes.containsSlice(org.ergoplatform.mining.groupElemToBytes(pk.value)))
  }

  def assetAmount(boxes: Seq[ErgoBoxCandidate]): TokensMap = {
    assetsByTokenId(boxes).map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
  }

  def toAssetMap(assetSeq: Seq[(TokenId, Long)]): TokensMap = {
    assetSeq
      .map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
      .toMap
  }

  def assetsByTokenId(boxes: Seq[ErgoBoxCandidate]): Map[TokenId, Long] = {
    boxes
      .flatMap(_.additionalTokens.toArray)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
      }
      .map(x => decodedTokenId(x._1) -> x._2)
  }

  def getUtxoState(implicit ctx: Ctx): UtxoState = getCurrentState.asInstanceOf[UtxoState]

  def getHeightOf(state: ErgoState[_])(implicit ctx: Ctx): Option[Int] = {
    getHistory.heightOf(scorex.core.versionToId(state.version))
  }

  def makeGenesisBlock(script: ProveDlog, assets: Seq[(TokenId, Long)] = Seq.empty)
                      (implicit ctx: Ctx): ErgoFullBlock = {
    makeNextBlock(getUtxoState, Seq(makeGenesisTx(script, assets)))
  }

  def makeGenesisTx(publicKey: ProveDlog, assetsIn: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
    val assets: Seq[(TokenId, Long)] = replaceNewAssetStub(assetsIn, inputs)
    ErgoMiner.collectRewards(Some(genesisEmissionBox),
      ErgoHistory.EmptyHistoryHeight,
      Seq.empty,
      publicKey,
      emission,
      Colls.fromArray(assets.toArray)).head
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     addressToReturn: ErgoAddress,
                     balanceToReturn: Long = 0,
                     assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    makeTx(boxesToSpend, emptyProverResult, balanceToReturn, addressToReturn.script, assets)
  }

  def makeTx(boxesToSpend: Seq[ErgoBox],
             proofToSpend: ProverResult,
             balanceToReturn: Long,
             scriptToReturn: ErgoTree,
             assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = boxesToSpend.map(box => Input(box.id, proofToSpend))
    val balanceToSpend = boxesToSpend.map(_.value).sum - balanceToReturn
    def creatingCandidate = new ErgoBoxCandidate(balanceToReturn, scriptToReturn, startHeight, replaceNewAssetStub(assets, inputs).toColl)
    val spendingOutput = if (balanceToSpend > 0) Some(new ErgoBoxCandidate(balanceToSpend, Constants.TrueLeaf, creationHeight = startHeight)) else None
    val creatingOutput = if (balanceToReturn > 0) Some(creatingCandidate) else None
    ErgoTransaction(inputs.toIndexedSeq, spendingOutput.toIndexedSeq ++ creatingOutput.toIndexedSeq)
  }

  private def replaceNewAssetStub(assets: Seq[(TokenId, Long)], inputs: Seq[Input]): Seq[(TokenId, Long)] = {
    def isNewAsset(tokenId: TokenId, value: Long): Boolean =  java.util.Arrays.equals(tokenId, newAssetIdStub)
    val (newAsset, spentAssets) = assets.partition((isNewAsset _).tupled)
    newAsset.map(Digest32 @@ inputs.head.boxId -> _._2) ++ spentAssets
  }

  def randomNewAsset: Seq[(TokenId, Long)] = Seq(newAssetIdStub -> randomLong())
  def assetsWithRandom(boxes: Seq[ErgoBox]): Seq[(TokenId, Long)] = randomNewAsset ++ assetsByTokenId(boxes)
  def badAssets: Seq[(TokenId, Long)] = additionalTokensGen.sample.value
}
