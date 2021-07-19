package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.util.Try

/**
  * Contains functions required by History to process Transactions and Proofs when we have them.
  * Prune modifiers older then blocksToKeep.
  */
trait FullBlockProcessor extends HeadersProcessor {

  import FullBlockProcessor._

  private var nonBestChainsCache = FullBlockProcessor.emptyCache

  def isInBestFullChain(id: ModifierId): Boolean = historyStorage.getIndex(chainStatusKey(id))
    .map(ByteArrayWrapper.apply)
    .contains(ByteArrayWrapper(FullBlockProcessor.BestChainMarker))

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] =
    historyStorage.getIndex(BestFullBlockKey).map(bytesToId)

  // todo: `getFullBlock` is frequently used to define whether some`header` have enough
  // todo: related sections - it would be far more efficient to keep such information in the indexes.
  protected def getFullBlock(h: Header): Option[ErgoFullBlock]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param newMod    - new modifier we are going to put in the history
    * @return ProgressInfo required for State to process to be consistent with the history
    */
  protected def processFullBlock(fullBlock: ErgoFullBlock,
                                 newMod: ErgoPersistentModifier): ProgressInfo[ErgoPersistentModifier] = {
    val bestFullChainAfter = calculateBestChain(fullBlock.header)
    val newBestBlockHeader = typedModifierById[Header](bestFullChainAfter.last).ensuring(_.isDefined)
    processing(ToProcess(fullBlock, newMod, newBestBlockHeader, bestFullChainAfter))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  private def isValidFirstFullBlock(header: Header): Boolean = {
    pruningProcessor.isHeadersChainSynced &&
      header.height == pruningProcessor.minimalFullBlockHeight &&
      bestFullBlockIdOpt.isEmpty
  }

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, Some(newBestBlockHeader), newBestChain)
      if isValidFirstFullBlock(fullBlock.header) =>

      val headers = headerChainBack(10, fullBlock.header, h => h.height == 1)
      val toApply = fullBlock +: newBestChain.tail
        .map(id => typedModifierById[Header](id).flatMap(getFullBlock))
        .takeWhile(_.isDefined)
        .flatten
      logStatus(Seq(), toApply, fullBlock, None)
      val additionalIndexes = toApply.map(b => chainStatusKey(b.id) -> FullBlockProcessor.BestChainMarker)
      updateStorage(newModRow, newBestBlockHeader.id, additionalIndexes)
      ProgressInfo(None, Seq.empty, headers.headers.dropRight(1) ++ toApply, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, Some(newBestBlockHeader), _)
      if bestFullBlockOpt.nonEmpty && isBetterChain(newBestBlockHeader.id) && isLinkable(fullBlock.header) =>

      val prevBest = bestFullBlockOpt.get

      //find chains starting from the forking point
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, newBestBlockHeader)
      val toRemove: Seq[ErgoFullBlock] = prevChain.tail.headers.flatMap(getFullBlock)
      val toApply: Seq[ErgoFullBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getFullBlock(h))
        .ensuring(_.lengthCompare(newChain.length - 1) == 0)

      // application of this block leads to full chain with higher score
      logStatus(toRemove, toApply, fullBlock, Some(prevBest))
      val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

      // insert updated chains statuses
      val additionalIndexes = toApply.map(b => chainStatusKey(b.id) -> FullBlockProcessor.BestChainMarker) ++
        toRemove.map(b => chainStatusKey(b.id) -> FullBlockProcessor.NonBestChainMarker)
      updateStorage(newModRow, newBestBlockHeader.id, additionalIndexes)

      // remove block ids which have no chance to be applied
      val minForkRootHeight = toApply.last.height - nodeSettings.keepVersions
      if (nonBestChainsCache.nonEmpty) nonBestChainsCache = nonBestChainsCache.dropUntil(minForkRootHeight)

      if (nodeSettings.isFullBlocksPruned) {
        val lastKept = pruningProcessor.updateBestFullBlock(fullBlock.header)
        val bestHeight: Int = newBestBlockHeader.height
        val diff = bestHeight - prevBest.header.height
        pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
      }
      ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
  }

  /**
    *
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
      val block = params.fullBlock
      if (block.header.height > fullBlockHeight - nodeSettings.keepVersions) {
        nonBestChainsCache = nonBestChainsCache.add(block.id, block.parentId, block.header.height)
      }
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq(), Seq(), params.fullBlock, None)
      historyStorage.insert(Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  /**
    * @return `true` if a given `header` is linkable to some existing full chain or
    *         contains original genesis block, `false` otherwise
    */
  private def isLinkable(header: Header): Boolean = {
    // todo check loop for possible inefficient chains (e.g. limit the loop depth)
    @tailrec
    def loop(id: ModifierId, height: Int, acc: Seq[ModifierId]): Seq[ModifierId] = {
      nonBestChainsCache.getParentId(id, height).orElse { // lookup block in the cache
        typedModifierById[Header](id) // lookup block in storage in case its not presented in the cache.
          .flatMap(h => if (!isInBestFullChain(id)) getFullBlock(h) else None)
          .map(_.parentId)
      } match {
        case Some(parentId) => loop(parentId, height - 1, parentId +: acc)
        case None => acc
      }
    }

    if (bestFullBlockIdOpt.exists(_ == header.parentId)) {
      true
    } else {
      // follow links back until main chain or absent section is reached
      val headOpt = loop(header.parentId, header.height - 1, Seq.empty).headOption
      headOpt.exists(_ == Header.GenesisParentId) ||
        headOpt.orElse(Some(header.parentId))
          .flatMap(id => typedModifierById[Header](id).flatMap(getFullBlock)) // check whether first block actually exists
          .isDefined
    }
  }

  /**
    * Finds all possible chains following a given `header`.
    */
  private def continuationChains(fromHeader: Header): Seq[Seq[ModifierId]] = {
    @tailrec
    def loop(currentHeight: Option[Int], acc: Seq[Seq[(ModifierId, ModifierId)]]): Seq[Seq[ModifierId]] = {
      val nextLevelBlocks = currentHeight.toList
        .flatMap { h =>
          val nextHeight = h + 1
          headerIdsAtHeight(nextHeight)
            .flatMap { id =>
              nonBestChainsCache.getParentId(id, nextHeight) // lookup block in the cache
                .map(parentId => id -> parentId)
                .orElse {
                  typedModifierById[Header](id) // lookup block in storage in case its not presented in the cache.
                    .flatMap(getFullBlock)
                    .map(b => b.id -> b.parentId)
                }
            }
        }
      if (nextLevelBlocks.isEmpty) {
        acc.map(chain => chain.map(_._1).reverse)
      } else {
        val updatedChains = nextLevelBlocks.flatMap { block =>
          acc.find(chain => chain.nonEmpty && (block._2 == chain.head._1)).map(c => block +: c)
        }
        val nonUpdatedChains = acc.filter(chain => !nextLevelBlocks.exists(_._2 == chain.head._1))
        loop(currentHeight.map(_ + 1), updatedChains ++ nonUpdatedChains)
      }
    }

    loop(heightOf(fromHeader.id), Seq(Seq(fromHeader.id -> fromHeader.parentId)))
  }

  /**
    * Finds best chain following a given `header`.
    */
  private def calculateBestChain(header: Header): Seq[ModifierId] = {
    continuationChains(header)
      .map(_.tail)
      .map(header.id +: _)
      .maxBy(c => scoreOf(c.last))
  }

  private def logStatus(toRemove: Seq[ErgoFullBlock],
                        toApply: Seq[ErgoFullBlock],
                        appliedBlock: ErgoFullBlock,
                        prevBest: Option[ErgoFullBlock]): Unit = {
    val toRemoveStr = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr = if (toApply.isEmpty) "" else {
        s"New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${ErgoHistory.heightOf(prevBest.map(_.header))}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers. $newStatusStr")
  }

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[Header](id))
      .flatMap(_.sectionIds.map(_._2))
    historyStorage.remove(toRemove)
  }

  private def updateStorage(newModRow: ErgoPersistentModifier,
                            bestFullHeaderId: ModifierId,
                            additionalIndexes: Seq[(ByteArrayWrapper, Array[Byte])] = Seq.empty): Unit = {
    val indicesToInsert = Seq(BestFullBlockKey -> idToBytes(bestFullHeaderId)) ++ additionalIndexes
    historyStorage.insert(indicesToInsert, Seq(newModRow))
      .ensuring(headersHeight >= fullBlockHeight, s"Headers height $headersHeight should be >= " +
        s"full height $fullBlockHeight")
  }

}

object FullBlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[ErgoPersistentModifier]]

  case class ToProcess(fullBlock: ErgoFullBlock,
                       newModRow: ErgoPersistentModifier,
                       newBestBlockHeaderOpt: Option[Header],
                       newBestChain: Seq[ModifierId])

  case class CacheBlock(id: ModifierId, height: Int)

  /**
    * Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied.
    */
  case class IncompleteFullChainCache(cache: TreeMap[CacheBlock, ModifierId]) {

    val nonEmpty: Boolean = cache.nonEmpty

    def getParentId(id: ModifierId, height: Int): Option[ModifierId] = cache.get(CacheBlock(id, height))

    def add(id: ModifierId, parentId: ModifierId, height: Int): IncompleteFullChainCache =
      IncompleteFullChainCache(cache.insert(CacheBlock(id, height), parentId))

    def dropUntil(height: Int): IncompleteFullChainCache =
      IncompleteFullChainCache(cache.dropWhile(_._1.height < height))
  }

  val BestChainMarker: Array[Byte] = Array(1: Byte)
  val NonBestChainMarker: Array[Byte] = Array(0: Byte)

  private implicit val ord: Ordering[CacheBlock] = Ordering[(Int, ModifierId)].on(x => (x.height, x.id))

  def emptyCache: IncompleteFullChainCache = IncompleteFullChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("main_chain".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

}
