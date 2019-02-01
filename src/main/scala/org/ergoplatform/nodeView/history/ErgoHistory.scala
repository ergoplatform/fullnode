package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.modifierprocessors._
import org.ergoplatform.nodeView.history.modifierprocessors.popow.{EmptyPoPoWProofsProcessor, FullPoPoWProofsProcessor}
import org.ergoplatform.nodeView.history.storage.{FilesObjectsStore, HistoryStorage}
import org.ergoplatform.settings._
import scorex.core.consensus.History
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.util.Try

/**
  * History implementation. It is processing persistent modifiers generated locally or coming from network.
  * Depending on chosen node settings, it will process modifiers in a different way, different processors define how to
  * process different type of modifiers.
  *
  * HeadersProcessor: processor of block headers. It's the same for all node settings
  * ADProofsProcessor: processor of ADProofs. ADProofs may
  *   1. Be downloaded from other nodes (ADState == true)
  *   2. Be calculated by using local state (ADState == false)
  *   3. Be ignored by history in light mode (verifyTransactions == false)
  * PoPoWProofsProcessor: processor of PoPoWProof. PoPoWProof may
  *   1. Be downloaded once during bootstrap from other peers (poPoWBootstrap == true)
  *   2. Be ignored by history (poPoWBootstrap == false)
  * BlockTransactionsProcessor: Processor of BlockTransactions. BlockTransactions may
  *   1. Be downloaded from other peers (verifyTransactions == true)
  *   2. Be ignored by history (verifyTransactions == false)
  */
trait ErgoHistory
  extends History[ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
    with ErgoHistoryReader {

  override type NVCT = ErgoHistory
  override protected lazy val requireProofs: Boolean = config.stateType.requireProofs

  def closeStorage(): Unit = historyStorage.close()

  /**
    * Append ErgoPersistentModifier to History if valid
    */
  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = {
    log.debug(s"Trying to append modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} to history")
    applicableTry(modifier).map { _ =>
      modifier match {
        case header: Header =>
          (this, process(header))
        case section: BlockSection =>
          (this, process(section))
        case poPoWProof: PoPoWProof =>
          (this, process(poPoWProof))
        case manifest: UtxoSnapshotManifest =>
          (this, process(manifest))
        case chunk: UtxoSnapshotChunk =>
          (this, process(chunk))
        case localSnapshot: UtxoSnapshot =>
          (this, process(localSnapshot))
      }
    }
  }

  /**
    * Mark modifier as valid
    */
  override def reportModifierIsValid(modifier: ErgoPersistentModifier): ErgoHistory = {
    log.debug(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    modifier match {
      case fb: ErgoFullBlock =>
        val nonMarkedIds = (fb.header.id +: fb.header.sectionIds.map(_._2))
          .filter(id => historyStorage.getIndex(validityKey(id)).isEmpty)

        if (nonMarkedIds.nonEmpty) {
          historyStorage.insert(validityKey(nonMarkedIds.head),
            nonMarkedIds.map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte))),
            Seq.empty)
        }
      case _ =>
        historyStorage.insert(validityKey(modifier.id),
          Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(1.toByte))),
          Seq.empty)
    }
    this
  }

  /**
    * Mark modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid from State point of view
    * @return ProgressInfo with next modifier to try to apply
    */
  @SuppressWarnings(Array("OptionGet", "TraversableHead"))
  override def reportModifierIsInvalid(modifier: ErgoPersistentModifier,
                                       progressInfo: ProgressInfo[ErgoPersistentModifier]
                                      ): (ErgoHistory, ProgressInfo[ErgoPersistentModifier]) = {
    log.debug(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val invalidatedIds = invalidatedHeaders.map(_.id).toSet
        val validityRow = invalidatedHeaders.flatMap(h => Seq(h.id, h.transactionsId, h.ADProofsId)
          .map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte))))
        log.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
        val bestHeaderIsInvalidated = bestHeaderIdOpt.exists(id => invalidatedIds.contains(id))
        val bestFullIsInvalidated = bestFullBlockIdOpt.exists(id => invalidatedIds.contains(id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required
            historyStorage.insert(validityKey(modifier.id), validityRow, Seq.empty)
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required
            val newBestHeader = loopHeightDown(headersHeight, id => !invalidatedIds.contains(id))
              .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
              .get

            if (!bestFullIsInvalidated) {
              //Only headers chain involved
              historyStorage.insert(validityKey(modifier.id),
                Seq(BestHeaderKey -> Algos.idToBAW(newBestHeader.id)),
                Seq.empty)
              this -> ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
            } else {
              // todo: last requirement could not be met in pruned full mode.
              val invalidatedChain: Seq[ErgoFullBlock] = bestFullBlockOpt.toSeq
                .flatMap(f => headerChainBack(fullBlockHeight + 1, f.header, h => !invalidatedIds.contains(h.id)).headers)
                .flatMap(h => getFullBlock(h))
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")

              val branchPoint = invalidatedChain.head
              val validChain: Seq[ErgoFullBlock] = {
                continuationHeaderChains(branchPoint.header,
                  h => getFullBlock(h).isDefined && !invalidatedIds.contains(h.id))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(h => getFullBlock(h))
              }

              val changedLinks = Seq(BestFullBlockKey -> Algos.idToBAW(validChain.last.id),
                BestHeaderKey -> Algos.idToBAW(newBestHeader.id))
              val toInsert = validityRow ++ changedLinks
              historyStorage.insert(validityKey(modifier.id), toInsert, Seq.empty)
              this -> ProgressInfo[ErgoPersistentModifier](Some(branchPoint.id), invalidatedChain.tail,
                validChain.tail, Seq.empty)
            }
        }
      case None =>
        //No headers become invalid. Just valid this modifier as invalid
        historyStorage.insert(validityKey(modifier.id),
          Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(0.toByte))),
          Seq.empty)
        this -> ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }
  }

  /**
    * @return header, that corresponds to modifier
    */
  protected def correspondingHeader(modifier: ErgoPersistentModifier): Option[Header] = modifier match {
    case h: Header => Some(h)
    case full: ErgoFullBlock => Some(full.header)
    case proof: ADProofs => typedModifierById[Header](proof.headerId)
    case txs: BlockTransactions => typedModifierById[Header](txs.headerId)
    case _ => None
  }

}

object ErgoHistory extends ScorexLogging {

  type Height = Int
  type Score = BigInt
  type Difficulty = BigInt
  type NBits = Long

  val EmptyHistoryHeight: Int = 0
  val GenesisHeight: Int = EmptyHistoryHeight + 1
  def heightOf(headerOpt: Option[Header]): Int = headerOpt.map(_.height).getOrElse(EmptyHistoryHeight)

  def historyDir(settings: ErgoSettings): File = {
    val dir = new File(s"${settings.directory}/history")
    dir.mkdirs()
    dir
  }

  def readOrGenerate(settings: ErgoSettings, ntp: NetworkTimeProvider): ErgoHistory = {
    val historyFolder = historyDir(settings)
    historyFolder.mkdirs()
    val indexStore = new LSMStore(historyFolder, keepVersions = 0)
    val objectsStore = new FilesObjectsStore(historyFolder.getAbsolutePath)
    val db = new HistoryStorage(indexStore, objectsStore, settings.cacheSettings)
    val nodeSettings = settings.nodeSettings

    val history: ErgoHistory = (nodeSettings.verifyTransactions, nodeSettings.PoPoWBootstrap) match {
      case (true, true) =>
        new ErgoHistory with FullBlockSectionProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }

      case (false, true) =>
        new ErgoHistory with EmptyBlockSectionProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }

      case (true, false) =>
        new ErgoHistory with FullBlockSectionProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }

      case (false, false) =>
        new ErgoHistory with EmptyBlockSectionProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
    }
    history
  }

}
