package org.ergoplatform.nodeView.mempool

import scorex.util.ModifierId


sealed trait MempoolProcessingOutcome

object MempoolProcessingOutcome {

  /**
    * Object signalling that a transaction is accepted to the memory pool
    */
  case object Accepted extends MempoolProcessingOutcome

  /**
    * Class signalling that a valid transaction was rejected as it is double-spending inputs of mempool transactions
    * and has no bigger weight (fee/byte) than them on average.
    * @param winnerTxIds - identifiers of transactions won in replace-by-fee auction
    */
  case class DoubleSpendingLoser(winnerTxIds: Set[ModifierId]) extends MempoolProcessingOutcome

  /**
    * Class signalling that a transaction declined from being accepted into the memory pool
    */
  case class Declined(e: Throwable) extends MempoolProcessingOutcome


  /**
    * Class signalling that a transaction turned out to be invalid when checked in the mempool
    */
  case class Invalidated(e: Throwable) extends MempoolProcessingOutcome

}
