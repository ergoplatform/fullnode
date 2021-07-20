package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.TrackedBox

import scala.collection.mutable

/**
  * Holds version-agnostic off-chain data (such as off-chain boxes) in runtime memory.
  * Needed to obtain wallet state in regards with unconfirmed transactions with no reprocessing them on each request.
  *
  * @param height           - latest processed block height
  * @param offChainBoxes    - boxes from off-chain transactions
  * @param onChainBalances  - on-chain balances snapshot (required to calculate off-chain indexes)
  */
case class OffChainRegistry(height: Int,
                            offChainBoxes: Seq[TrackedBox],
                            onChainBalances: Seq[Balance]) {

  import org.ergoplatform.nodeView.wallet.IdUtils._

  /**
    * Off-chain index considering on-chain balances.
    */
  val digest: WalletDigest = {
    val offChainBalances = offChainBoxes.map(Balance.apply)
    val balance = offChainBalances.map(_.value).sum + onChainBalances.map(_.value).sum
    val tokensBalance = (offChainBalances ++ onChainBalances)
      .flatMap(_.assets)
      .foldLeft(mutable.LinkedHashMap.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc += id -> (acc.getOrElse(id, 0L) + amt)
      }
    WalletDigest(height, balance, tokensBalance.toSeq)
  }

  /**
    * Update on receiving new off-chain transaction.
    */
  def updateOnTransaction(newBoxes: Seq[TrackedBox], spentIds: Seq[EncodedBoxId], scans: Seq[Scan]): OffChainRegistry = {
    val unspentCertain = offChainBoxes.flatMap{x: TrackedBox =>
      val spent = spentIds.contains(x.boxId)

      if(x.scans.size > 1 || (x.scans.size == 1 && x.scans.head > Constants.PaymentsScanId)) {
        val leave = scans.exists(s => x.scans.contains(s.scanId) && !s.removeOffchain)
        if(leave) Some(x) else None
      } else {
        if(spent) None else Some(x)
      }
    } ++ newBoxes
    val onChainBalancesUpdated = onChainBalances.filterNot(x => spentIds.contains(x.id))
    this.copy(
      offChainBoxes = unspentCertain.distinct,
      onChainBalances = onChainBalancesUpdated
    )
  }

  /**
    * Update balances snapshot according to a new block applied
    *
    * @param newHeight       - processed block height
    * @param allCertainBoxes -  all the unspent boxes to the moment
    * @param onChainIds      - ids of all boxes which became on-chain in result of a current block application
    */
  def updateOnBlock(newHeight: Int,
                    allCertainBoxes: Seq[TrackedBox],
                    onChainIds: Seq[EncodedBoxId]): OffChainRegistry = {
    val updatedOnChainBalances = allCertainBoxes.map(Balance.apply)
    val cleanedOffChainBoxes = offChainBoxes.filterNot(b => onChainIds.contains(b.boxId))
    this.copy(
      height = newHeight,
      offChainBoxes = cleanedOffChainBoxes,
      onChainBalances = updatedOnChainBalances
    )
  }

}

object OffChainRegistry {

  def empty: OffChainRegistry =
    OffChainRegistry(ErgoHistory.EmptyHistoryHeight, Seq.empty, Seq.empty)

  def init(walletRegistry: WalletRegistry):OffChainRegistry = {
    val unspent = walletRegistry.unspentBoxes(PaymentsScanId)
    val h = walletRegistry.fetchDigest().height
    OffChainRegistry(h, Seq.empty, unspent.map(Balance.apply))
  }

}
