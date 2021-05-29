package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoApp
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationResult, ValidationState}
import scorex.db.ByteArrayWrapper
import scorex.util._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ToDownloadProcessor with ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  protected val settings: ErgoSettings

  val powScheme: AutolykosPowScheme

  val nipopowAlgos: NipopowAlgos = new NipopowAlgos(powScheme)

  // Maximum time in future block header may have
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings)

  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  // todo for performance reasons we may just use key like s"score$id" but this will require to redownload blockchain
  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected[history] def validityKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.getIndex(BestHeaderKey).map(bytesToId)

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  /**
    * @return height of best header
    */
  def headersHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @return height of best header with all block sections
    */
  def fullBlockHeight: Int = bestFullBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @param id - id of ErgoPersistentModifier
    * @return height of modifier with such id if is in History
    */
  def heightOf(id: ModifierId): Option[Int] = historyStorage.getIndex(headerHeightKey(id))
    .map(Ints.fromByteArray)

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h)).contains(id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).contains(h.id)

  /**
    * @param h - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: Header): ProgressInfo[ErgoPersistentModifier] = synchronized {
    val dataToInsert: (Seq[(ByteArrayWrapper, Array[Byte])], Seq[ErgoPersistentModifier]) = toInsert(h)

    historyStorage.insert(dataToInsert._1, dataToInsert._2)

    bestHeaderIdOpt match {
      case Some(bestHeaderId) =>
        // If we verify transactions, we don't need to send this header to state.
        // If we don't and this is the best header, we should send this header to state to update state root hash
        val toProcess = if (nodeSettings.verifyTransactions || !(bestHeaderId == h.id)) Seq.empty else Seq(h)
        ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
      case None =>
        log.error("Should always have best header after header application")
        ErgoApp.forceStopApplication()
    }
  }

  /**
    * Data to add to and remove from the storage to process this modifier
    */
  private def toInsert(h: Header): (Seq[(ByteArrayWrapper, Array[Byte])], Seq[ErgoPersistentModifier]) = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    val score = scoreOf(h.parentId).getOrElse(BigInt(0)) + requiredDifficulty
    val bestRow: Seq[(ByteArrayWrapper, Array[Byte])] =
      if (score > bestHeadersChainScore) Seq(BestHeaderKey -> idToBytes(h.id)) else Seq.empty
    val scoreRow = headerScoreKey(h.id) -> score.toByteArray
    val heightRow = headerHeightKey(h.id) -> Ints.toByteArray(h.height)
    val headerIdsRow = if (score > bestHeadersChainScore) {
      if (h.isGenesis) log.info(s"Processing genesis header ${h.encodedId}")
      bestBlockHeaderIdsRow(h, score)
    } else {
      orphanedBlockHeaderIdsRow(h, score)
    }

    (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, Seq(h))
  }

  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, Array[Byte])] = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> (headerIdsAtHeight(h.height) :+ h.id).flatMap(idToBytes).toArray)
  }

  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, Array[Byte])] = {
    val prevHeight = headersHeight
    log.info(s"New best header ${h.encodedId} with score $score. New height ${h.height}, old height $prevHeight")
    val self: (ByteArrayWrapper, Array[Byte]) =
      heightIdsKey(h.height) -> (Seq(h.id) ++ headerIdsAtHeight(h.height)).flatMap(idToBytes).toArray
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, Array[Byte])] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => id != header.id)
      heightIdsKey(header.height) -> (Seq(header.id) ++ otherIds).flatMap(idToBytes).toArray
    }
    forkIds :+ self
  }

  /** Validates given header
    *
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = new HeaderValidator().validate(header).toTry

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(Header.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(-1))

  /**
    * @param id - header id
    * @return score of header with such id if is in History
    */
  def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.getIndex(headerScoreKey(id))
    .map(BigInt.apply)

  /**
    * Get main chain header id
    *
    * Optimized version of headerIdsAtHeight(height).headOption
    *
    * @param height - height to get header id at
    * @return - header id or None
    */
  def bestHeaderIdAtHeight(height: Int): Option[ModifierId] = {
    historyStorage.getIndex(heightIdsKey(height: Int)).map { bs =>
      // in 99% cases bs.length == 32 (no orphaned headers)
      if (bs.length == 32) {
        bytesToId(bs)
      } else {
        bytesToId(bs.take(32))
      }
    }
  }

  def bestHeaderAtHeight(h: Int): Option[Header] = bestHeaderIdAtHeight(h).flatMap { id =>
    typedModifierById[Header](id)
  }

  /**
    * @note this method implementation should be changed along with `bestHeaderIdAtHeight`
    *
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    historyStorage.getIndex(heightIdsKey(height: Int))
      .getOrElse(Array()).grouped(32).map(bytesToId).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: scala.collection.mutable.Buffer[Header]): Seq[Header] = {
      if (acc.lengthCompare(limit) == 0 || until(header)) {
        acc
      } else {
        typedModifierById[Header](header.parentId) match {
          case Some(parent: Header) =>
            loop(parent, acc += parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc += header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) {
      HeaderChain(ArrayBuffer.empty)
    } else {
      HeaderChain(loop(startHeader, ArrayBuffer.empty += startHeader).reverse)
    }
  }

  /**
    * Find first header with height <= `height` which id satisfies condition `p`
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = {
    headerIdsAtHeight(height).find(id => p(id)).flatMap(id => typedModifierById[Header](id)) match {
      case Some(header) => Some(header)
      case None if height > 0 => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  private def bestHeadersChainScore: BigInt = bestHeaderIdOpt.flatMap(id => scoreOf(id)).getOrElse(0)

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  /**
    * Calculate difficulty for the next block
    *
    * @param parent - latest block
    * @return - difficulty for the next block
    */
  def requiredDifficultyAfter(parent: Header): Difficulty = {
    if (parent.height == settings.chainSettings.voting.version2ActivationHeight || parent.height + 1 == settings.chainSettings.voting.version2ActivationHeight) {
      // Set difficulty for version 2 activation height (where specific difficulty is needed due to PoW change)
      settings.chainSettings.initialDifficultyVersion2
    } else {
      //todo: it is slow to read thousands headers from database for each header
      //todo; consider caching here
      //todo: https://github.com/ergoplatform/ergo/issues/872
      val parentHeight = parent.height
      val heights = difficultyCalculator.previousHeadersRequiredForRecalculation(parentHeight + 1)
        .ensuring(_.last == parentHeight)
      if (heights.lengthCompare(1) == 0) {
        difficultyCalculator.calculate(Seq(parent))
      } else {
        val chain = headerChainBack(heights.max - heights.min + 1, parent, _ => false)
        val headers = chain.headers.filter(p => heights.contains(p.height))
        difficultyCalculator.calculate(headers)
      }
    }
  }

  class HeaderValidator extends ScorexEncoding {

    private def validationState: ValidationState[Unit] = ModifierValidator(ErgoValidationSettings.initial)

    def validate(header: Header): ValidationResult[Unit] = {
      if (header.isGenesis) {
        validateGenesisBlockHeader(header)
      } else {
        val parentOpt = typedModifierById[Header](header.parentId)
        parentOpt map { parent =>
          validateChildBlockHeader(header, parent)
        } getOrElse {
          validationState.validate(hdrParent, condition = false, Algos.encode(header.parentId))
        }
      }
    }

    private def validateGenesisBlockHeader(header: Header): ValidationResult[Unit] = {
      validationState
        .validateEqualIds(hdrGenesisParent, header.parentId, Header.GenesisParentId)
        .validateOrSkipFlatten(hdrGenesisFromConfig, chainSettings.genesisId, (id: ModifierId) => id.equals(header.id))
        .validate(hdrGenesisHeight, header.height == GenesisHeight, header.toString)
        .validateNoFailure(hdrPoW, powScheme.validate(header))
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, chainSettings.initialDifficulty)
        .validateNot(alreadyApplied, historyStorage.contains(header.id), header.id.toString)
        .validate(hdrTooOld, fullBlockHeight < nodeSettings.keepVersions, heightOf(header.parentId).toString)
        .validate(hdrFutureTimestamp, header.timestamp - timeProvider.time() <= MaxTimeDrift, s"${header.timestamp} vs ${timeProvider.time()}")
        .result
    }

    /**
      * Validate non-genesis block header.
      */
    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult[Unit] = {
      validationState
        .validate(hdrNonIncreasingTimestamp, header.timestamp > parent.timestamp, s"${header.timestamp} > ${parent.timestamp}")
        .validate(hdrHeight, header.height == parent.height + 1, s"${header.height} vs ${parent.height}")
        .validateNoFailure(hdrPoW, powScheme.validate(header))
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, requiredDifficultyAfter(parent))
        .validate(hdrTooOld, heightOf(header.parentId).exists(h => fullBlockHeight - h < nodeSettings.keepVersions), heightOf(header.parentId).toString)
        .validateSemantics(hdrParentSemantics, isSemanticallyValid(header.parentId))
        .validate(hdrFutureTimestamp, header.timestamp - timeProvider.time() <= MaxTimeDrift, s"${header.timestamp} vs ${timeProvider.time()}")
        .validateNot(alreadyApplied, historyStorage.contains(header.id), header.id.toString)
        .validate(hdrCheckpointV2, checkpointV2Condition(header), "Wrong V2 checkpoint")
        .result
    }

    /**
      * Helper method to check v2 checkpoint (first v2 block)
      */
    private def checkpointV2Condition(header: Header): Boolean = {
      if (header.height == 417792 && settings.networkType.isMainNet) {
        header.id == "0ba60a7db44877aade553beb05200f7d67b586945418d733e455840d283e0508"
      } else {
        true
      }
    }

  }

}
