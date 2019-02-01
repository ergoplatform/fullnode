package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.modifierprocessors._
import org.ergoplatform.nodeView.history.modifierprocessors.popow.PoPoWProofsProcessor
import org.ergoplatform.settings.{Algos, ChainSettings, NodeConfigurationSettings}
import scorex.core.consensus.History._
import scorex.core.consensus.{HistoryReader, ModifierSemanticValidity}
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Read-only copy of ErgoHistory
  */
trait ErgoHistoryReader
  extends HistoryReader[ErgoPersistentModifier, ErgoSyncInfo]
    with HeadersProcessor
    with PoPoWProofsProcessor
    with UtxoSnapshotChunkProcessor
    with UtxoSnapshotManifestProcessor
    with UtxoSnapshotProcessor
    with BlockSectionProcessor
    with ScorexLogging
    with ScorexEncoding {

  protected val chainSettings: ChainSettings
  protected val config: NodeConfigurationSettings

  protected[history] val historyStorage: HistoryStorage

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet (from a chain or a PoPoW proof).
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for fullnode regime after initial bootstrap.
    */
  def bestFullBlockOpt: Option[ErgoFullBlock] =
    bestFullBlockIdOpt.flatMap(id => typedModifierById[Header](id)).flatMap(getFullBlock)

  /**
    * @param id - modifier id
    * @return semantically valid ErgoPersistentModifier with the given id it is in history
    */
  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    if (isSemanticallyValid(id) != ModifierSemanticValidity.Invalid) {
      historyStorage.modifierById(id)
    } else {
      None
    }
  }.ensuring(_.forall(_.id == id), s"Modifier ${Algos.encode(id)} id is incorrect")

  /** Get modifier of expected type by its identifier
    *
    * @param id - modifier id
    * @tparam T - expected Type
    * @return semantically valid ErgoPersistentModifier of type T with the given id it is in history
    */
  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T) => Some(m)
    case _ => None
  }

  override def contains(id: ModifierId): Boolean = historyStorage.contains(id)

  /**
    * Id of best block to mine
    */
  override def openSurfaceIds(): Seq[ModifierId] = bestFullBlockIdOpt.orElse(bestHeaderIdOpt).toSeq

  /**
    * Check, that it's possible to apply modifier to history
    */
  def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(info: ErgoSyncInfo): HistoryComparisonResult = {
    bestHeaderIdOpt match {
      case Some(id) if info.lastHeaderIds.lastOption.contains(id) =>
        //Our best header is the same as other node best header
        Equal
      case Some(id) if info.lastHeaderIds.contains(id) =>
        //Our best header is in other node best chain, but not at the last position
        Older
      case Some(_) if info.lastHeaderIds.isEmpty =>
        //Other history is empty, our contain some headers
        Younger
      case Some(_) =>
        //We are on different forks now.
        if (info.lastHeaderIds.view.reverse.exists(m => contains(m))) {
          //Return Younger, because we can send blocks from our fork that other node can download.
          Younger
        } else {
          //We don't have any of id's from other's node sync info in history.
          //We don't know whether we can sync with it and what blocks to send in Inv message.
          //Assume it is older and far ahead from us
          Older
        }
      case None if info.lastHeaderIds.isEmpty =>
        //Both nodes do not keep any blocks
        Equal
      case None =>
        //Our history is empty, other contain some headers
        Older
    }
  }

  /**
    * @param info other's node sync info
    * @param size max return size
    * @return Ids of headerss, that node with info should download and apply to synchronize
    */
  @SuppressWarnings(Array("OptionGet", "TraversableHead"))
  override def continuationIds(info: ErgoSyncInfo, size: Int): Option[ModifierIds] = Try {
    if (isEmpty) {
      info.startingPoints
    } else if (info.lastHeaderIds.isEmpty) {
      val heightFrom = Math.min(headersHeight, size + ErgoHistory.EmptyHistoryHeight)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[Header](startId).get
      val headers = headerChainBack(size, startHeader, _ => false)
        .ensuring(_.headers.exists(_.isGenesis), "Should always contain genesis header")
      headers.headers.flatMap(h => Seq((Header.modifierTypeId, h.id)))
    } else {
      val ids = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight = heightOf(lastHeaderInOurBestChain).get
      val heightFrom = Math.min(headersHeight, theirHeight + size)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[Header](startId).get
      val headerIds = headerChainBack(size, startHeader, h => h.parentId == lastHeaderInOurBestChain)
        .headers.map(h => Header.modifierTypeId -> h.id)
      headerIds
    }
  }.toOption

  /**
    *
    * @param header     - header to start
    * @param withFilter - condition to satisfy
    * @return all possible forks, starting from specified header and satisfying withFilter condition
    */
  protected[history] def continuationHeaderChains(header: Header, withFilter: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec
    def loop(currentHeight: Option[Int], acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextLevelHeaders = currentHeight.toList
        .flatMap { h => headerIdsAtHeight(h + 1) }
        .flatMap { id => typedModifierById[Header](id) }
        .filter(withFilter)
      if (nextLevelHeaders.isEmpty) {
        acc.map(chain => chain.reverse)
      } else {
        val updatedChains = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId == chain.head.id)).map(c => h +: c)
        }
        val nonUpdatedChains = acc.filter(chain => !nextLevelHeaders.exists(_.parentId == chain.head.id))
        loop(currentHeight.map(_ + 1), updatedChains ++ nonUpdatedChains)
      }
    }

    loop(heightOf(header.id), Seq(Seq(header)))
  }


  /**
    * @return Node ErgoSyncInfo
    */
  override def syncInfo: ErgoSyncInfo = if (isEmpty) {
    ErgoSyncInfo(Seq.empty)
  } else {
    ErgoSyncInfo(lastHeaders(ErgoSyncInfo.MaxBlockIds).headers.map(_.id))
  }

  /**
    * Return last count headers from best headers chain if exist or chain up to genesis otherwise
    */
  def lastHeaders(count: Int, offset: Int = 0): HeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, b => false).drop(offset)).getOrElse(HeaderChain.empty)


  /**
    * @return ids of count headers starting from offset
    */
  def headerIdsAt(offset: Int = 0, limit: Int): Seq[ModifierId] = (offset until (limit + offset))
    .flatMap(h => headerIdsAtHeight(h).headOption)

  override def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = {
    modifier match {
      case header: Header =>
        validate(header)
      case m: BlockSection =>
        validate(m)
      case m: PoPoWProof =>
        validate(m)
      case manifest: UtxoSnapshotManifest =>
        validate(manifest)
      case chunk: UtxoSnapshotChunk =>
        validate(chunk)
      case localSnapshot: UtxoSnapshot =>
        validate(localSnapshot)
      case m: Any =>
        Failure(new Error(s"Modifier $m has incorrect type"))
    }
  }

  def getFullBlock(header: Header): Option[ErgoFullBlock] = {
    (typedModifierById[BlockTransactions](header.transactionsId),
      typedModifierById[Extension](header.extensionId),
      typedModifierById[ADProofs](header.ADProofsId)) match {
      case (Some(txs), Some(ext), Some(proofs)) => Some(ErgoFullBlock(header, txs, ext, Some(proofs)))
      case (Some(txs), Some(ext), None) if !config.stateType.requireProofs =>
        Some(ErgoFullBlock(header, txs, ext, None))
      case _ => None
    }
  }

  /**
    * Return headers, required to apply to reach header2 if you are at header1 position.
    *
    * @param startHeaderOpt - initial position
    * @param finalHeader    - header you should reach
    * @return (Modifier it required to rollback first, header chain to apply)
    */
  def chainToHeader(startHeaderOpt: Option[Header], finalHeader: Header): (Option[ModifierId], HeaderChain) = {
    startHeaderOpt match {
      case Some(h1) =>
        val (prevChain, newChain) = commonBlockThenSuffixes(h1, finalHeader)
        (prevChain.headOption.map(_.id), newChain.tail)
      case None =>
        (None, headerChainBack(finalHeader.height + 1, finalHeader, _ => false))
    }
  }

  /**
    * Find common block and subchains from common block to header1 and header2
    *
    * @param header1 : Header - header in first subchain
    * @param header2 : Header - header in second subchain
    * @return (chain from common block to header1, chain from common block to header2)
    */
  protected[history] def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain) = {
    assert(contains(header1) && contains(header2), "Should never call this function for non-existing headers")
    val heightDiff = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDiff)
      if (r._1.head == r._2.head) {
        r
      } else {
        val biggerOther = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) {
          loop(biggerOther.size, biggerOther)
        } else {
          throw new Error(s"Common point not found for headers $header1 and $header2")
        }
      }
    }

    loop(2, HeaderChain(Seq(header2)))
  }

  protected[history] def commonBlockThenSuffixes(otherChain: HeaderChain,
                                                 startHeader: Header,
                                                 limit: Int): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id == h.id)

    val ourChain = headerChainBack(limit, startHeader, until)
    val commonBlock = ourChain.head
    val commonBlockThenSuffixes = otherChain.takeAfter(commonBlock)
    (ourChain, commonBlockThenSuffixes)
  }

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = {
    historyStorage.getIndex(validityKey(modifierId)) match {
      case Some(b) if b.data.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(b) if b.data.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if contains(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case m =>
        log.error(s"Incorrect validity status: $m")
        ModifierSemanticValidity.Absent
    }
  }

}
