package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoApp
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.{Algos, NodeConfigurationSettings, Parameters}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationResult}
import scorex.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ToDownloadProcessor with ScorexLogging with ScorexEncoding {

  val powScheme: AutolykosPowScheme

  protected val historyStorage: HistoryStorage

  protected val config: NodeConfigurationSettings

  lazy val difficultyCalculator = new LinearDifficultyControl(chainSettings.blockInterval,
    chainSettings.useLastEpochs, chainSettings.epochLength)

  //Maximum time in future block header may contain
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(Header.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(-1))

  private val charsetName = "UTF-8"

  // todo for performance reasons we may just use key like s"score$id" but this will require to redownload blockchain
  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(charsetName) ++ idToBytes(id)))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(charsetName) ++ idToBytes(id)))

  protected[history] def validityKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(charsetName) ++ idToBytes(id)))

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.getIndex(BestHeaderKey).map(w => bytesToId(w.data))

  def realDifficulty(h: Header): Difficulty = powScheme.realDifficulty(h)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

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
    .map(b => Ints.fromByteArray(b.data))

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h)).contains(id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).contains(h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  /**
    * @param h - header to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: Header): ProgressInfo[ErgoPersistentModifier] = {
    val dataToInsert: (Seq[(ByteArrayWrapper, ByteArrayWrapper)], Seq[ErgoPersistentModifier]) = toInsert(h)

    historyStorage.insert(Algos.idToBAW(h.id), dataToInsert._1, dataToInsert._2)

    bestHeaderIdOpt match {
      case Some(bestHeaderId) =>
        // If we verify transactions, we don't need to send this header to state.
        // If we don't and this is the best header, we should send this header to state to update state root hash
        val toProcess = if (config.verifyTransactions || !(bestHeaderId == h.id)) Seq.empty else Seq(h)
        ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
      case None =>
        log.error("Should always have best header after header application")
        ErgoApp.forceStopApplication()
    }
  }

  /**
    * Data, we should add and remove from the storage to process this modifier
    */
  private def toInsert(h: Header): (Seq[(ByteArrayWrapper, ByteArrayWrapper)], Seq[ErgoPersistentModifier]) = {
    val requiredDifficulty: Difficulty = h.requiredDifficulty
    if (h.isGenesis) {
      genesisToInsert(h, requiredDifficulty)
    } else {
      nonGenesisToInsert(h, requiredDifficulty)
    }
  }

  /**
    * Data to insert for regular block
    */
  private def nonGenesisToInsert(h: Header, requiredDifficulty: Difficulty) = {
    val score = scoreOf(h.parentId).get + requiredDifficulty
    val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (score > bestHeadersChainScore) Seq(BestHeaderKey -> Algos.idToBAW(h.id)) else Seq.empty
    val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
    val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
    val headerIdsRow = if (score > bestHeadersChainScore) {
      bestBlockHeaderIdsRow(h, score)
    } else {
      orphanedBlockHeaderIdsRow(h, score)
    }
    (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, modifiersToInsert(h))
  }

  /**
    * Data to insert for genesis block
    */
  private def genesisToInsert(h: Header, requiredDifficulty: Difficulty) = {
    log.info(s"Initialize header chain with genesis header ${h.encodedId}")
    (Seq(
      BestHeaderKey -> Algos.idToBAW(h.id),
      heightIdsKey(GenesisHeight) -> Algos.idToBAW(h.id),
      headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(GenesisHeight)),
      headerScoreKey(h.id) -> ByteArrayWrapper(requiredDifficulty.toByteArray)),
      modifiersToInsert(h))
  }

  private def modifiersToInsert(header: Header): Seq[ErgoPersistentModifier] = {
    if (java.util.Arrays.equals(header.extensionRoot, Algos.emptyMerkleTreeRoot)) {
      // extension is empty, generate it and insert to history
      Seq(header, Extension(header.id, Seq()))
    } else {
      Seq(header)
    }
  }

  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatMap(idToBytes).toArray))
  }

  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty) = {
    val prevHeight = headersHeight
    log.info(s"New best header ${h.encodedId} with score $score. New height ${h.height}, old height $prevHeight")
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatMap(idToBytes).toArray)
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => id != header.id)
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatMap(idToBytes).toArray)
    }
    forkIds :+ self
  }

  /**
    *
    * @param header - header we're going to remove from history
    * @return ids to remove, new data to apply
    */
  protected def reportInvalid(header: Header): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val modifierId = header.id
    val payloadModifiers = Seq(header.transactionsId, header.ADProofsId).filter(id => historyStorage.contains(id))
      .map(id => Algos.idToBAW(id))

    val toRemove = Seq(headerScoreKey(modifierId), Algos.idToBAW(modifierId)) ++ payloadModifiers
    val bestHeaderKeyUpdate = if (bestHeaderIdOpt.contains(modifierId)) {
      Seq(BestHeaderKey -> Algos.idToBAW(header.parentId))
    } else {
      Seq.empty
    }
    val bestFullBlockKeyUpdate = if (bestFullBlockIdOpt.contains(modifierId)) {
      Seq(BestFullBlockKey -> Algos.idToBAW(header.parentId))
    } else {
      Seq.empty
    }
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)
  }

  /** Validates given header
    *
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = new HeaderValidator().validate(header).toTry

  /**
    * @param id - header id
    * @return score of header with such id if is in History
    */
  def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.getIndex(headerScoreKey(id))
    .map(b => BigInt(b.data))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    historyStorage.getIndex(heightIdsKey(height: Int))
      .map(w => w.data).getOrElse(Array()).grouped(32).map(bytesToId).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.lengthCompare(limit) == 0 || until(header)) {
        acc
      } else {
        typedModifierById[Header](header.parentId) match {
          case Some(parent: Header) =>
            loop(parent, acc :+ parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc :+ header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) {
      HeaderChain(Seq.empty)
    } else {
      HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
    }
  }

  /**
    * Find first header with the best height <= `height` which id satisfies condition `p`
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

  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val parentHeight = parent.height
    val heights = difficultyCalculator.previousHeadersRequiredForRecalculation(parentHeight + 1)
      .ensuring(_.last == parentHeight)
    if (heights.lengthCompare(1) == 0) {
      difficultyCalculator.calculate(Seq(parent))
    } else {
      val chain = headerChainBack(heights.max - heights.min + 1, parent, (_: Header) => false)
      val headers = chain.headers.filter(p => heights.contains(p.height))
      difficultyCalculator.calculate(headers)
    }
  }

  class HeaderValidator extends ModifierValidator with ScorexEncoding {

    def validate(header: Header): ValidationResult[Unit] = {
      if (header.isGenesis) {
        validateGenesisBlockHeader(header)
      } else {
        val parentOpt = typedModifierById[Header](header.parentId)
        parentOpt map { parent =>
          validateChildBlockHeader(header, parent)
        } getOrElse {
          error(s"Parent header with id ${Algos.encode(header.parentId)} is not defined")
        }
      }
    }

    /**
      * Check that non-zero votes extracted from block header are correct
      */
    private def checkVotes(header: Header): Unit = {
      val votes: Array[Byte] = header.votes.filter(_ != Parameters.NoParameter)
      val epochStarts = header.votingStarts(chainSettings.voting.votingLength)
      val votesCount = votes.count(_ != Parameters.SoftFork)
      if (votesCount > Parameters.ParamVotesCount) throw new Error(s"Too many votes $votesCount")

      val prevVotes = mutable.Buffer[Byte]()
      votes.foreach { v =>
        if (prevVotes.contains(v)) throw new Error(s"Double vote in ${votes.mkString}")
        if (prevVotes.contains((-v).toByte)) throw new Error(s"Contradictory votes in ${votes.mkString}")
        if (epochStarts && !Parameters.parametersDescs.contains(v)) throw new Error("Incorrect vote proposed")
        prevVotes += v
      }
    }

    private def validateGenesisBlockHeader(header: Header): ValidationResult[Unit] = {
      accumulateErrors
        .validateEqualIds(header.parentId, Header.GenesisParentId) { detail =>
          fatal(s"Genesis block should have genesis parent id. $detail")
        }
        .validate(chainSettings.genesisId.forall {
          _ == Algos.encode(header.id)
        }) {
          fatal(s"Expected genesis block id is ${chainSettings.genesisId.getOrElse("")}," +
            s" got genesis block with id ${Algos.encode(header.id)}")
        }
        .validate(bestHeaderIdOpt.isEmpty) {
          fatal("Trying to append genesis block to non-empty history")
        }
        .validate(header.height == GenesisHeight) {
          fatal(s"Height of genesis block $header is incorrect")
        }
        .demandNoThrow(checkVotes(header), "Incorrect votes")
        .result
    }

    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult[Unit] = {
      failFast
        .validate(header.timestamp > parent.timestamp) {
          fatal(s"Header timestamp ${header.timestamp} is not greater than parents ${parent.timestamp}")
        }
        .validate(header.height == parent.height + 1) {
          fatal(s"Header height ${header.height} is not greater by 1 than parents ${parent.height}")
        }
        .validate(realDifficulty(header) >= header.requiredDifficulty) {
          fatal(s"Block difficulty ${realDifficulty(header)} is less than required ${header.requiredDifficulty}")
        }
        .validateEquals(header.requiredDifficulty)(requiredDifficultyAfter(parent)) { detail =>
          fatal(s"Incorrect required difficulty. $detail")
        }
        .validate(heightOf(header.parentId).exists(h => fullBlockHeight - h < config.keepVersions)) {
          fatal(s"Trying to apply too old header at height ${heightOf(header.parentId)}")
        }
        .validate(powScheme.validate(header).isSuccess) {
          fatal(s"Wrong proof-of-work solution for $header")
        }
        .validateSemantics(isSemanticallyValid(header.parentId)) {
          fatal("Parent header is marked as semantically invalid")
        }
        .validate(header.timestamp - timeProvider.time() <= MaxTimeDrift) {
          error(s"Header timestamp ${header.timestamp} is too far in future from now ${timeProvider.time()}")
        }
        .validateNot(historyStorage.contains(header.id)) {
          error(s"Header ${header.id} is already in history")
        }
        .demandNoThrow(checkVotes(header), "Incorrect votes")
        .result
    }

  }

}
