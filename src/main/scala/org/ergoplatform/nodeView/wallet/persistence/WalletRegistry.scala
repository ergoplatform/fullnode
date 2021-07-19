package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedTokenId, encodedTokenId}
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletTransactionSerializer}
import org.ergoplatform.settings.{Algos, ErgoSettings, WalletSettings}
import org.ergoplatform.wallet.{AssetUtils, Constants}
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import scorex.core.VersionTag
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success, Try}
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.wallet.transactions.TransactionBuilder
import scorex.util.encode.Base16

import scala.collection.mutable

/**
  * Provides an access to version-sensitive wallet-specific indexes:
  *
  * * current wallet status (height, balances)
  * * wallet-related transactions
  * * boxes, spent or not
  *
  */
class WalletRegistry(store: LDBVersionedStore)(ws: WalletSettings) extends ScorexLogging {

  import WalletRegistry._

  private val keepHistory = ws.keepSpentBoxes

  /**
    * Close wallet registry storage
    */
  def close(): Unit = {
    store.close()
  }

  /**
    * Read wallet-related box with metadata
    *
    * @param id - box identifier (the same as Ergobox identifier)
    * @return wallet related box if it is stored in the database, None otherwise
    */
  def getBox(id: BoxId): Option[TrackedBox] = {
    store.get(boxKey(id)).flatMap(bs => TrackedBoxSerializer.parseBytesTry(bs).toOption)
  }


  /**
    * Read wallet-related boxes with metadata, see getBox()
    *
    * @param ids - box identifier
    * @return wallet related boxes (optional result for each box)
    */
  def getBoxes(ids: Seq[BoxId]): Seq[Option[TrackedBox]] = {
    ids.map(id => getBox(id))
  }

  /**
    * Read unspent boxes which belong to all the scans
    *
    * @return sequences of all the unspent boxes from the database
    */
  def allUnspentBoxes(): Seq[TrackedBox] = {
    store.getRange(firstUnspentBoxKey, lastUnspentBoxKey)
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Read unspent boxes which belong to a scan with given id
    *
    * @param scanId - scan identifier
    * @return sequences of scan-related unspent boxes found in the database
    */
  def unspentBoxes(scanId: ScanId): Seq[TrackedBox] = {
    store
      .getRange(firstScanBoxSpaceKey(scanId), lastScanBoxSpaceKey(scanId))
      .flatMap { case (_, boxId) => getBox(ADKey @@ boxId) }
  }

  /**
    * Read spent boxes which belong to a scan with given id
    *
    * @param scanId - scan identifier
    * @return sequences of scan-related spent boxes found in the database
    */
  def spentBoxes(scanId: ScanId): Seq[TrackedBox] = {
    store.getRange(firstSpentScanBoxSpaceKey(scanId), lastSpentScanBoxSpaceKey(scanId))
      .flatMap { case (_, boxId) =>
        getBox(ADKey @@ boxId)
      }
  }

  /**
    * Unspent boxes belong to the wallet (payments scan)
    */
  def walletUnspentBoxes(): Seq[TrackedBox] = unspentBoxes(Constants.PaymentsScanId)

  /**
    * Spent boxes belong to the wallet (payments scan)
    */
  def walletSpentBoxes(): Seq[TrackedBox] = spentBoxes(Constants.PaymentsScanId)

  /**
    * Read wallet boxes, both spent or not
    *
    * @param scanId scan identifier
    * @return sequence of scan-related boxes
    */
  def confirmedBoxes(scanId: ScanId): Seq[TrackedBox] = {
    unspentBoxes(scanId) ++ spentBoxes(scanId)
  }

  /**
    * Read boxes belong to the payment scan, both spent or not
    *
    * @return sequence of (P2PK-payment)-related boxes
    */
  def walletConfirmedBoxes(): Seq[TrackedBox] = confirmedBoxes(Constants.PaymentsScanId)

  /**
    * Read transaction with wallet-related metadata
    *
    * @param id - transaction identifier
    * @return
    */
  def getTx(id: ModifierId): Option[WalletTransaction] = {
    store.get(txKey(id)).flatMap(r => WalletTransactionSerializer.parseBytesTry(r).toOption)
  }

  /**
    * Read all the wallet-related transactions
    *
    * @return all the transactions for all the scans
    */
  def allWalletTxs(): Seq[WalletTransaction] = {
    store.getRange(FirstTxSpaceKey, LastTxSpaceKey)
      .flatMap { case (_, txBytes) =>
        WalletTransactionSerializer.parseBytesTry(txBytes).toOption
      }
  }

  /**
    * Read wallet-related transactions for certain heights
    *
    * @param heightFrom - height to start from (inclusive)
    * @param heightTo - height to finish at (inclusive)
    * @return - wallet transactions for the heights range provided
    */
  def walletTxsBetween(heightFrom: Height, heightTo: Height): Seq[WalletTransaction] = {
    val firstKey = firstIncludedScanTransactionSpaceKey(Constants.PaymentsScanId, heightFrom)
    val lastKey = lastIncludedScanTransactionSpaceKey(Constants.PaymentsScanId, heightTo)

    // Get wallet transactions from heightFrom (inclusive) to heightTo (inclusive)
    val range = store.getRange(firstKey, lastKey)
    range.flatMap { case (_, txId) =>
      store.get(txKey(txId)) match {
        case Some(txBytes) => WalletTransactionSerializer.parseBytesTry(txBytes) match {
          case Success(tx) =>
            Some(tx)
          case Failure(t) =>
            log.error(s"Transaction ${Base16.encode(txId)} can't be read from the db", t); None
        }
        case None =>
          log.error(s"Transaction ${Base16.encode(txId)} is found in indexes but not db"); None
      }
    }
  }

  /**
    * Read aggregate wallet information
    *
    * @return wallet digest
    */
  def fetchDigest(): WalletDigest = {
    store.get(RegistrySummaryKey)
      .flatMap(r => WalletDigestSerializer.parseBytesTry(r).toOption)
      .getOrElse(WalletDigest.empty)
  }


  /**
    * Update aggregate wallet information
    */
  def updateDigest(bag: KeyValuePairsBag)(updateF: WalletDigest => WalletDigest): KeyValuePairsBag = {
    val digest = fetchDigest()
    putDigest(bag, updateF(digest))
  }

  /**
    *
    * Updates indexes according to data extracted from a block and performs versioned update.
    *
    * @param scanResults - block scan data (outputs created and spent along with corresponding transactions)
    * @param blockId     - block identifier
    * @param blockHeight - block height
    */
  def updateOnBlock(scanResults: ScanResults, blockId: ModifierId, blockHeight: Int): Try[Unit] = Try {

    // first, put newly created outputs and related transactions into key-value bag
    val bag1 = putBoxes(KeyValuePairsBag.empty, scanResults.outputs)
    val bag2 = putTxs(bag1, scanResults.relatedTransactions)

    // process spent boxes
    val spentBoxesWithTx = scanResults.inputsSpent.map(t => t.inputTxId -> t.trackedBox)
    val bag3 = processSpentBoxes(bag2, spentBoxesWithTx, blockHeight)

    // and update wallet digest
    val bag4 = updateDigest(bag3) { case WalletDigest(height, wBalance, wTokensSeq) =>
      if (height + 1 != blockHeight) {
        log.error(s"Blocks were skipped during wallet scanning, from $height until $blockHeight")
      }
      val spentWalletBoxes = spentBoxesWithTx.map(_._2).filter(_.scans.contains(PaymentsScanId))
      val spentAmt = spentWalletBoxes.map(_.box.value).sum
      val spentTokensAmt = spentWalletBoxes
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }
      val receivedTokensAmt = scanResults.outputs.filter(_.scans.contains(PaymentsScanId))
        .flatMap(_.box.additionalTokens.toArray)
        .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
          acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
        }

      val wTokens = mutable.LinkedHashMap(wTokensSeq: _*)

      val increasedTokenBalances = receivedTokensAmt.foldLeft(wTokens) { case (acc, (encodedId, amt)) =>
        acc += encodedId -> (acc.getOrElse(encodedId, 0L) + amt)
      }

      val newTokensBalance = spentTokensAmt
        .foldLeft(increasedTokenBalances) { case (acc, (encodedId, amt)) =>
          val decreasedAmt = acc.getOrElse(encodedId, 0L) - amt
          if (decreasedAmt > 0) {
            acc += encodedId -> decreasedAmt
          } else {
            acc -= encodedId
          }
        }

      val receivedAmt = scanResults.outputs.filter(_.scans.contains(PaymentsScanId)).map(_.box.value).sum
      val newBalance = wBalance + receivedAmt - spentAmt
      require(
        (newBalance >= 0 && newTokensBalance.forall(_._2 >= 0)) || ws.testMnemonic.isDefined,
        "Balance could not be negative")
      WalletDigest(blockHeight, newBalance, newTokensBalance.toSeq)
    }

    bag4.transact(store, idToBytes(blockId))
  }

  def rollback(version: VersionTag): Try[Unit] =
    store.rollbackTo(scorex.core.versionToBytes(version))

  /**
    * Transits used boxes to a spent state or simply deletes them depending on a settings.
    */
  private[persistence] def processSpentBoxes(bag: KeyValuePairsBag,
                                             spentBoxes: Seq[(ModifierId, TrackedBox)],
                                             spendingHeight: Int): KeyValuePairsBag = {
    if (keepHistory) {
      val outSpent: Seq[TrackedBox] = spentBoxes.flatMap { case (_, tb) =>
        getBox(tb.box.id).orElse {
          bag.toInsert.find(_._1.sameElements(boxKey(tb))).flatMap { case (_, tbBytes) =>
            TrackedBoxSerializer.parseBytesTry(tbBytes).toOption
          } match {
            case s@Some(_) => s
            case None =>
              log.warn(s"Output spent hasn't found in the wallet: ${Algos.encode(tb.box.id)}, " +
                s"could be okay if it was created before wallet init")
              None
          }
        }: Option[TrackedBox]
      }

      val updatedBoxes = outSpent.map { tb =>
        val spendingTxIdOpt = spentBoxes
          .find { case (_, x) => x.box.id.sameElements(tb.box.id) }
          .map(_._1)
        tb.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = spendingTxIdOpt)
      }

      val bagBeforePut = removeBoxes(bag, spentBoxes.map(_._2))
      putBoxes(bagBeforePut, updatedBoxes)
    } else {
      removeBoxes(bag, spentBoxes.map(_._2))
    }
  }

  /**
    * Updates scans of a box stored in the wallet database,
    * puts the box into the database if it is not there
    * removes the box from the database if its there and scanIds are empty
    *
    * @param newScans - ids of new scans box should be associated with
    * @param box - box to be updated (new version)
    * @return
    */
  def updateScans(newScans: Set[ScanId], box: ErgoBox): Try[Unit] = Try {
    val bag0 = KeyValuePairsBag.empty
    val oldBox = getBox(box.id) // read old version from the database
    val oldScans = oldBox.map(_.scans).getOrElse(Set.empty)

    val newBox = TrackedBox(box, box.creationHeight, newScans)

    val bag1 = (oldScans.isEmpty, newScans.isEmpty) match {
      case (false, false) =>
        // replace scans of the box by removing it along with indexes related to old scans,
        // and then adding the box with indexes related to the new scans
        putBox(removeBox(bag0, oldBox.get), newBox)
      case (false, true) =>
        // if new scans are empty, remove the box along with indexes
        removeBox(bag0, oldBox.get)
      case (true, false) =>
        // if old scans are empty, add the box along with indexes
        putBox(bag0, newBox)
      case (true, true) =>
        //old and new scans are empty, can't do anything useful
        throw new Exception("Can't remove a box which does not exist")
    }

    // Flag showing that box has been added to the payments app (p2pk-wallet) or removed from it
    // If true, we need to update wallet digest
    val digestChanged = (oldScans.contains(Constants.PaymentsScanId) || newScans.contains(Constants.PaymentsScanId)) &&
                        !(oldScans.contains(Constants.PaymentsScanId) && newScans.contains(Constants.PaymentsScanId))

    val bag2 = if (digestChanged) {
      val digest = fetchDigest()

      val walletAssets = mutable.LinkedHashMap(digest.walletAssetBalances :_*)
      val boxAssets = TransactionBuilder.collTokensToMap(box.additionalTokens)

      val updDigest = if (!oldScans.contains(Constants.PaymentsScanId) && newScans.contains(Constants.PaymentsScanId)) {
        AssetUtils.mergeAssetsMut(walletAssets, boxAssets) //mutating digest!
        WalletDigest(
          digest.height,
          digest.walletBalance + box.value,
          walletAssets.toArray[(EncodedTokenId, Long)])
      } else if (oldScans.contains(Constants.PaymentsScanId) && !newScans.contains(Constants.PaymentsScanId)) {
        //mutating digest! exception can be thrown here
        AssetUtils.subtractAssetsMut(walletAssets, boxAssets)
        WalletDigest(
          digest.height,
          digest.walletBalance - box.value,
          walletAssets.toArray[(EncodedTokenId, Long)])
      } else {
        throw new Exception(s"Wallet can't update digest for a box with old scans $oldScans, new ones $newScans")
      }
      putDigest(bag1, updDigest)
    } else {
      bag1
    }

    bag2.transact(store, store.lastVersionID.getOrElse(scorex.util.Random.randomBytes(32)))
  }

  /**
    * Remove association between an application and a box.
    * Please note that in case of rollback association remains removed!
    *
    * @param boxId  box identifier
    * @param scanId scan identifier
    */
  def removeScan(boxId: BoxId, scanId: ScanId): Try[Unit] = {
    getBox(boxId) match {
      case Some(tb) =>
        val newScans = tb.scans - scanId
        updateScans(newScans, tb.box)

      case None => Failure(new Exception(s"No box with id ${Algos.encode(boxId)} found in the wallet database"))
    }
  }
}

object WalletRegistry {

  import scorex.db.ByteArrayUtils._

  val PreGenesisStateVersion: Array[Byte] = idToBytes(PreGenesisHeader.id)

  def registryFolder(settings: ErgoSettings): File = new File(s"${settings.directory}/wallet/registry")

  def apply(settings: ErgoSettings): WalletRegistry = {
    val dir = registryFolder(settings)
    dir.mkdirs()

    val store = new LDBVersionedStore(dir, settings.nodeSettings.keepVersions)

    // Create pre-genesis state checkpoint
    if (!store.versionIdExists(PreGenesisStateVersion)) store.update(PreGenesisStateVersion, Seq.empty, Seq.empty)

    new WalletRegistry(store)(settings.walletSettings)
  }


  private val BoxKeyPrefix: Byte = 0x01
  private val TxKeyPrefix: Byte = 0x02

  // box indexes prefixes
  private val UnspentIndexPrefix: Byte = 0x03
  private val SpentIndexPrefix: Byte = 0x04
  private val InclusionHeightScanBoxPrefix: Byte = 0x07

  // tx index prefixes
  private val InclusionHeightScanTxPrefix: Byte = 0x08

  private val FirstTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(0: Byte)
  private val LastTxSpaceKey: Array[Byte] = TxKeyPrefix +: Array.fill(32)(-1: Byte)

  // All the unspent boxes range, dependless on scan
  private val firstUnspentBoxKey: Array[Byte] = UnspentIndexPrefix +: Array.fill(34)(0: Byte)
  private val lastUnspentBoxKey: Array[Byte] = UnspentIndexPrefix +: Array.fill(34)(-1: Byte)

  /** Performance optimized helper, which avoid unnecessary allocations and creates the resulting
    * key bytes directly from the given parameters.
    * It is allocation and boxing free.
    *
    * @return prefix | scanId | Array.fill(32)(suffix)  bytes packed in an array
    */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, suffix: Byte): Array[Byte] = {
    val res = new Array[Byte](35) // 1 + 2 + 32
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putReplicated(res, pos = 3, n = 32, suffix)
    res
  }

  /** Same as [[composeKey()]] where suffix is given by id. */
  private[persistence] final def composeKeyWithId(prefix: Byte, scanId: ScanId, suffixId: Array[Byte]): Array[Byte] = {
    val res = new Array[Byte](3 + suffixId.length) // 1 byte for prefix + 2 for scanId
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putBytes(res, pos = 3, suffixId)
    res
  }

  /** Same as [[composeKey()]] with additional height parameter. */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, height: Int, suffix: Byte): Array[Byte] = {
    val res = new Array[Byte](39) // 1 byte for prefix + 2 for scanId + 4 for height + 32 for suffix
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    putReplicated(res, 7, 32, suffix)
    res
  }

  /** Same as [[composeKey()]] with additional height parameter. */
  private[persistence] final def composeKey(prefix: Byte, scanId: ScanId, height: Int): Array[Byte] = {
    val res = new Array[Byte](39) // 1 byte for prefix + 2 for scanId + 4 for height + 32 for suffix
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    res
  }


  /** Same as [[composeKey()]] with additional height parameter and suffix given by id. */
  private[persistence] final def composeKeyWithHeightAndId(prefix: Byte, scanId: ScanId,
                                                           height: Int, suffixId: Array[Byte]): Array[Byte] = {
    val res = new Array[Byte](7 + suffixId.length) // 1 byte for prefix + 2 for scanId + 4 for height
    res(0) = prefix
    putShort(res, pos = 1, scanId)
    putInt(res, pos = 3, height)
    putBytes(res, 7, suffixId)
    res
  }

  private def firstScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, 0)

  private def lastScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, -1)

  private def firstSpentScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, 0)

  private def lastSpentScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(SpentIndexPrefix, scanId, -1)

  private def firstIncludedScanBoxSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, height, 0)

  private def lastIncludedScanBoxSpaceKey(scanId: ScanId): Array[Byte] =
    composeKey(UnspentIndexPrefix, scanId, Int.MaxValue, -1)

  private def firstIncludedScanTransactionSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanTxPrefix, scanId, height)

  private def lastIncludedScanTransactionSpaceKey(scanId: ScanId, height: Int): Array[Byte] =
    composeKey(InclusionHeightScanTxPrefix, scanId, height, -1)

  private val RegistrySummaryKey: Array[Byte] = Array(0x02: Byte)

  private def boxKey(trackedBox: TrackedBox): Array[Byte] = BoxKeyPrefix +: trackedBox.box.id

  private def boxKey(id: BoxId): Array[Byte] = BoxKeyPrefix +: id

  private def txKey(id: ModifierId): Array[Byte] = TxKeyPrefix +: idToBytes(id)

  private def txKey(id: Array[Byte]): Array[Byte] = TxKeyPrefix +: id

  private def boxToKvPair(box: TrackedBox) = boxKey(box) -> TrackedBoxSerializer.toBytes(box)

  private def spentIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val prefix = if (trackedBox.isSpent) SpentIndexPrefix else UnspentIndexPrefix
    composeKeyWithId(prefix, scanId, trackedBox.box.id)
  }

  private def inclusionHeightScanBoxIndexKey(scanId: ScanId, trackedBox: TrackedBox): Array[Byte] = {
    val inclusionHeight = trackedBox.inclusionHeightOpt.getOrElse(0)
    composeKeyWithHeightAndId(InclusionHeightScanBoxPrefix, scanId, inclusionHeight, trackedBox.box.id)
  }

  private def boxIndexKeys(box: TrackedBox): Seq[Array[Byte]] = {
    box.scans.toSeq.flatMap { scanId =>
      Seq(
        spentIndexKey(scanId, box),
        inclusionHeightScanBoxIndexKey(scanId, box)
      )
    }
  }

  private def boxIndexes(box: TrackedBox): Seq[(Array[Byte], Array[Byte])] = {
    boxIndexKeys(box).map(k => k -> box.box.id)
  }

  private[persistence] def putBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val scanIndexUpdates = boxIndexes(box)
    val newKvPairs = scanIndexUpdates :+ boxToKvPair(box)
    bag.copy(toInsert = bag.toInsert ++ newKvPairs)
  }

  private[persistence] def putBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => putBox(b, box) }
  }

  private[persistence] def removeBox(bag: KeyValuePairsBag, box: TrackedBox): KeyValuePairsBag = {
    val boxKeys = boxIndexKeys(box) :+ boxKey(box)

    bag.toInsert.find(_._1.sameElements(boxKey(box))) match {
      case Some((_, _)) =>
        bag.copy(toInsert = bag.toInsert.filterNot { case (k, _) =>
          boxKeys.exists(_.sameElements(k))
        })
      case None =>
        bag.copy(toRemove = bag.toRemove ++ boxKeys)
    }
  }

  private[persistence] def removeBoxes(bag: KeyValuePairsBag, boxes: Seq[TrackedBox]): KeyValuePairsBag = {
    boxes.foldLeft(bag) { case (b, box) => removeBox(b, box) }
  }

  private def inclusionHeightScanTxIndexKey(scanId: ScanId, tx: WalletTransaction): Array[Byte] = {
    val inclusionHeight = tx.inclusionHeight
    composeKeyWithHeightAndId(InclusionHeightScanTxPrefix, scanId, inclusionHeight, tx.idBytes)
  }

  private def txIndexKeys(tx: WalletTransaction): Seq[Array[Byte]] = {
    tx.scanIds.map { scanId =>
      inclusionHeightScanTxIndexKey(scanId, tx)
    }
  }

  private def txToKvPairs(tx: WalletTransaction): Seq[(Array[Byte], Array[Byte])] = {
    txIndexKeys(tx).map(k => k -> tx.idBytes) :+
      (txKey(tx.id) -> WalletTransactionSerializer.toBytes(tx))
  }

  private[persistence] def putTx(bag: KeyValuePairsBag, wtx: WalletTransaction): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert ++ txToKvPairs(wtx))
  }

  private[persistence] def putTxs(bag: KeyValuePairsBag, txs: Seq[WalletTransaction]): KeyValuePairsBag = {
    bag.copy(toInsert = bag.toInsert ++ txs.flatMap(txToKvPairs))
  }

  private[persistence] def removeTxs(bag: KeyValuePairsBag, txs: Seq[WalletTransaction]): KeyValuePairsBag = {
    bag.copy(toRemove = bag.toRemove ++ txs.flatMap(txToKvPairs).map(_._1))
  }

  private[persistence] def putDigest(bag: KeyValuePairsBag, digest: WalletDigest): KeyValuePairsBag = {
    val registryBytes = WalletDigestSerializer.toBytes(digest)
    bag.copy(toInsert = bag.toInsert :+ (RegistrySummaryKey, registryBytes))
  }
}

/**
  * This class collects data for versioned database update
  *
  * @param toInsert - key-value pairs to write to the database
  * @param toRemove - keys to remove from the database
  */
case class KeyValuePairsBag(toInsert: Seq[(Array[Byte], Array[Byte])],
                            toRemove: Seq[Array[Byte]]) {

  /**
    * Applies non-versioned transaction to a given `store`.
    *
    */
  def transact(store: LDBVersionedStore): Unit = transact(store, None)

  /**
    * Applies versioned transaction to a given `store`.
    */
  def transact(store: LDBVersionedStore, version: Array[Byte]): Unit = transact(store, Some(version))

  private def transact(store: LDBVersionedStore, versionOpt: Option[Array[Byte]]): Unit =
    if (toInsert.nonEmpty || toRemove.nonEmpty) {
      store.update(versionOpt.getOrElse(scorex.utils.Random.randomBytes()), toRemove, toInsert)
    }

}

object KeyValuePairsBag {

  def empty: KeyValuePairsBag = KeyValuePairsBag(Seq.empty, Seq.empty)

}
