package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.modifierprocessors.FullBlockProcessor.{BlockProcessing, ToProcess}
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.util.{ModifierId, bytesToId}

import scala.util.Try

/**
  * Contains functions required by History to process Transactions and Proofs when we have them.
  * Prune modifiers older then blocksToKeep.
  */
trait FullBlockProcessor extends HeadersProcessor {

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] = historyStorage.getIndex(BestFullBlockKey)
    .map(w => bytesToId(w.data))

  protected def getFullBlock(h: Header): Option[ErgoFullBlock]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, withFilter: Header => Boolean): Seq[Seq[Header]]

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param newMod    - new modifier we are going to put in history
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processFullBlock(fullBlock: ErgoFullBlock,
                                 newMod: ErgoPersistentModifier): ProgressInfo[ErgoPersistentModifier] = {
    // TODO this is very inefficient - in most cases we do not need to calculate `bestFullChain`
    val bestFullChain = calculateBestFullChain(fullBlock)
    val newBestAfterThis = bestFullChain.last.header
    processing(ToProcess(fullBlock, newMod, newBestAfterThis, config.blocksToKeep, bestFullChain))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  protected def isValidFirstFullBlock(header: Header): Boolean = {
    pruningProcessor.isHeadersChainSynced &&
      header.height == pruningProcessor.minimalFullBlockHeight &&
      bestFullBlockIdOpt.isEmpty
  }

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestAfterThis, _, toApply)
      if isValidFirstFullBlock(fullBlock.header) =>

      val headers = headerChainBack(10, fullBlock.header, h => h.height == 1)
      logStatus(Seq(), toApply, fullBlock, None)
      updateStorage(newModRow, newBestAfterThis.id)
      ProgressInfo(None, Seq.empty, headers.headers.dropRight(1) ++ toApply, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess@ToProcess(fullBlock, newModRow, newBestBlock, blocksToKeep, _)
      if bestFullBlockOpt.nonEmpty && isBetterChain(newBestBlock.id) =>

      val prevBestBlock = bestFullBlockOpt.get
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBestBlock.header, newBestBlock)
      val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
      val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))

      if (toApply.lengthCompare(newChain.length - 1) != 0) {
        //block have higher score but is not linkable to full chain
        nonBestBlock(toProcess)
      } else {
        //application of this block leads to full chain with higher score
        logStatus(toRemove, toApply, fullBlock, Some(prevBestBlock))
        val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

        updateStorage(newModRow, newBestBlock.id)

        if (blocksToKeep >= 0) {
          val lastKept = pruningProcessor.updateBestFullBlock(fullBlock.header)
          val bestHeight: Int = newBestBlock.height
          val diff = bestHeight - prevBestBlock.header.height
          pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  /**
    * @param id - id of a header to compare
    * @return `true`, if block with id `id` is better, than current best block, `false` otherwise.
    */
  private def isBetterChain(id: ModifierId): Boolean = {
    (bestFullBlockIdOpt.flatMap(bfi => scoreOf(bfi)), scoreOf(id)) match {
      case (Some(prevBestScore), Some(score)) if score > prevBestScore => true
      case _ => false
    }
  }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq(), Seq(), params.fullBlock, None)
      historyStorage.insert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def calculateBestFullChain(fb: ErgoFullBlock): Seq[ErgoFullBlock] = {
    val continuations = continuationHeaderChains(fb.header, h => getFullBlock(h).nonEmpty).map(_.tail)
    val chains = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten)
    chains.map(c => fb +: c).maxBy(c => scoreOf(c.last.id))
  }

  private def logStatus(toRemove: Seq[ErgoFullBlock],
                        toApply: Seq[ErgoFullBlock],
                        appliedBlock: ErgoFullBlock,
                        prevBest: Option[ErgoFullBlock]): Unit = {
    val toRemoveStr = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${ErgoHistory.heightOf(prevBest.map(_.header))}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[Header](id))
      .flatMap(_.sectionIds.map(_._2))
    historyStorage.remove(toRemove)
  }

  private def updateStorage(newModRow: ErgoPersistentModifier,
                            bestFullHeaderId: ModifierId): Unit = {
    val indicesToInsert = Seq(BestFullBlockKey -> Algos.idToBAW(bestFullHeaderId))
    historyStorage.insert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
      .ensuring(headersHeight >= fullBlockHeight, s"Headers height $headersHeight should be >= " +
        s"full height $fullBlockHeight")
  }

  private def storageVersion(newModRow: ErgoPersistentModifier) = Algos.idToBAW(newModRow.id)

}

object FullBlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[ErgoPersistentModifier]]

  case class ToProcess(fullBlock: ErgoFullBlock,
                       newModRow: ErgoPersistentModifier,
                       newBestAfterThis: Header,
                       blocksToKeep: Int,
                       bestFullChain: Seq[ErgoFullBlock])
}
