package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings}
import scorex.core.transaction.MemoryPool
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, bytesToId}
import OrderedTxPool.weighted

import scala.annotation.tailrec
import scala.util.Try


/**
  * Immutable memory pool implementation.
  *
  * @param pool     - Ordered transaction pool. Acts as related transaction storage, and is
  *                 used for implementing all transaction-related methods
  * @param stats    - Mempool statistics, that allows to track
  *                 information about mempool's state and transactions in it.
  */
class ErgoMemPool private[mempool](pool: OrderedTxPool, private[mempool] val stats : MemPoolStatistics)(implicit settings: ErgoSettings)
  extends MemoryPool[ErgoTransaction, ErgoMemPool] with ErgoMemPoolReader {

  import EmissionRules.CoinsInOneErgo

  private implicit val monetarySettings: MonetarySettings = settings.chainSettings.monetary

  override type NVCT = ErgoMemPool

  override def size: Int = pool.size

  override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = pool.get(modifierId)

  override def take(limit: Int): Iterable[ErgoTransaction] = pool.orderedTransactions.values.take(limit)

  override def getAll: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ids.flatMap(pool.get)

  /**
    * Returns all transactions resided in pool sorted by weight in descending order
    */
  override def getAllPrioritized: Seq[ErgoTransaction] = pool.orderedTransactions.values.toSeq

  override def put(tx: ErgoTransaction): Try[ErgoMemPool] = put(Seq(tx))

  override def put(txs: Iterable[ErgoTransaction]): Try[ErgoMemPool] = Try {
    putWithoutCheck(txs.filterNot(tx => pool.contains(tx.id)))
  }

  override def putWithoutCheck(txs: Iterable[ErgoTransaction]): ErgoMemPool = {
    val updatedPool = txs.toSeq.distinct.foldLeft(pool) { case (acc, tx) => acc.put(tx) }
    new ErgoMemPool(updatedPool, stats)
  }

  override def remove(tx: ErgoTransaction): ErgoMemPool = {
    val wtx = pool.transactionsRegistry.get(tx.id)
    val updStats = wtx.map(wgtx => stats.add(System.currentTimeMillis(), wgtx))
      .getOrElse(MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))
    new ErgoMemPool(pool.remove(tx), updStats)
  }

  override def filter(condition: ErgoTransaction => Boolean): ErgoMemPool = {
    new ErgoMemPool(pool.filter(condition), stats)
  }

  /**
    * Invalidate transaction and delete it from pool
    *
    * @param tx - Transaction to invalidate
    */
  def invalidate(tx: ErgoTransaction): ErgoMemPool = {
    new ErgoMemPool(pool.invalidate(tx), stats)
  }

  /**
    * @return inputs spent by the mempool transactions
    */
  override def spentInputs: Iterator[BoxId] = pool.inputs.keysIterator

  // Check if transaction is double-spending inputs spent in the mempool.
  // If so, the new transacting is replacing older ones if it has bigger weight (fee/byte) than them on average.
  // Otherwise, the new transaction being rejected.
  private def acceptIfNoDoubleSpend(tx: ErgoTransaction): (ErgoMemPool, MempoolProcessingOutcome) = {
    val doubleSpendingWtxs = tx.inputs.flatMap { inp =>
      pool.inputs.get(inp.boxId)
    }.toSet

    if(doubleSpendingWtxs.nonEmpty) {
      val ownWtx = weighted(tx)
      val doubleSpendingTotalWeight = doubleSpendingWtxs.map(_.weight).sum / doubleSpendingWtxs.size
      if (ownWtx.weight > doubleSpendingTotalWeight) {
        val doubleSpendingTxs = doubleSpendingWtxs.map(wtx => pool.orderedTransactions(wtx)).toSeq
        new ErgoMemPool(pool.put(tx).remove(doubleSpendingTxs), stats) -> MempoolProcessingOutcome.Accepted
      } else {
        this -> MempoolProcessingOutcome.DoubleSpendingLoser(doubleSpendingWtxs.map(_.id))
      }
    } else {
      new ErgoMemPool(pool.put(tx), stats) -> MempoolProcessingOutcome.Accepted
    }
  }

  def process(tx: ErgoTransaction, state: ErgoState[_]): (ErgoMemPool, MempoolProcessingOutcome) = {
    val fee = extractFee(tx)
    val minFee = settings.nodeSettings.minimalFeeAmount
    val canAccept = pool.canAccept(tx)

    if (fee >= minFee) {
      if (canAccept) {
        state match {
          case utxo: UtxoState =>
            // Allow proceeded transaction to spend outputs of pooled transactions.
            utxo.withTransactions(getAll).validate(tx).fold(
              ex => new ErgoMemPool(pool.invalidate(tx), stats) -> MempoolProcessingOutcome.Invalidated(ex),
              _ => acceptIfNoDoubleSpend(tx)
            )
          case validator: TransactionValidation[ErgoTransaction@unchecked] =>
            // transaction validation currently works only for UtxoState, so this branch currently
            // will not be triggered probably
            validator.validate(tx).fold(
              ex => new ErgoMemPool(pool.invalidate(tx), stats) -> MempoolProcessingOutcome.Invalidated(ex),
              _ => acceptIfNoDoubleSpend(tx)
            )
          case _ =>
            // Accept transaction in case of "digest" state. Transactions are not downloaded in this mode from other
            // peers, thus such transactions can come from the local wallet only.
            acceptIfNoDoubleSpend(tx)
        }
      } else {
        this -> MempoolProcessingOutcome.Declined(
          new Exception(s"Pool can not accept transaction ${tx.id}, it is invalidated earlier or the pool is full"))
      }
    } else {
      this -> MempoolProcessingOutcome.Declined(
        new Exception(s"Min fee not met: ${minFee.toDouble / CoinsInOneErgo} ergs required, " +
          s"${fee.toDouble / CoinsInOneErgo} ergs given")
      )
    }
  }

  def weightedTransactionIds(limit: Int): Seq[WeightedTxId] = pool.orderedTransactions.keysIterator.take(limit).toSeq

  private def extractFee(tx: ErgoTransaction): Long =
    ErgoState.boxChanges(Array(tx))._2
      .filter(_.ergoTree == settings.chainSettings.monetary.feeProposition)
      .map(_.value)
      .sum

  /**
    * Get average fee for the specified wait time interval
    *
    * @param expectedWaitTimeMinutes - maximal amount of time for which transaction can be kept in mempool
    * @param txSize                  - size of transaction (in bytes)
    * @return recommended fee value for transaction to be proceeded in specified time
    */
  def getRecommendedFee(expectedWaitTimeMinutes: Int, txSize: Int): Long = {
    @tailrec def loop(waitMinutes: Int): Option[Long] =
      Try(stats.histogram(waitMinutes)).toOption match {
        case Some(bin) if bin.nTxns != 0 => Some(bin.totalFee / bin.nTxns * txSize / 1024)
        case _ if waitMinutes < expectedWaitTimeMinutes => loop(waitMinutes + 1)
        case _ => None
      }

    loop(waitMinutes = 0).getOrElse(settings.nodeSettings.minimalFeeAmount)
  }

  /**
    * Calculate position in mempool corresponding to the specified fee and
    * estimate time of serving this transaction based on average rate of placing
    * transactions in blockchain
    * @param txFee  - transaction fee
    * @param txSize - size of transaction (in bytes)
    * @return average time for this transaction to be placed in block
    */
  def getExpectedWaitTime(txFee : Long, txSize : Int): Long  = {
    // Create dummy transaction entry
    val feePerKb = txFee * 1024 / txSize
    val dummyModifierId = bytesToId(Array.fill(32)(0.toByte))
    val wtx = WeightedTxId(dummyModifierId, feePerKb, feePerKb, 0)

    // Find position of entry in mempool
    val posInPool = pool.orderedTransactions.keySet.until(wtx).size

    // Time since statistics measurement interval (needed to calculate average tx rate)
    val elapsed = System.currentTimeMillis() - stats.startMeasurement
    if (stats.takenTxns != 0) {
      elapsed * posInPool / stats.takenTxns
    } else {
      0
    }
  }
}

object ErgoMemPool {

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[ErgoTransaction]

  /**
    * Create empty mempool
    * @param settings - node settings (to get mempool settings from)
    * @return empty mempool
    */
  def empty(settings: ErgoSettings): ErgoMemPool =
    new ErgoMemPool(OrderedTxPool.empty(settings),
      MemPoolStatistics(System.currentTimeMillis(), 0, System.currentTimeMillis()))(settings)

}
