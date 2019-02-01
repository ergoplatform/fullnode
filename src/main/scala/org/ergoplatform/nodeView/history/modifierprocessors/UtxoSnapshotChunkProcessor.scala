package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.util.{Failure, Try}

trait UtxoSnapshotChunkProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  protected val PendingManifestIdKey: ByteArrayWrapper

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  protected val LastSnapshotAppliedHeightKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(Constants.HashLength)(UtxoSnapshot.modifierTypeId))

  protected val PendingChunksQtyKey: ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("pending-chunks".getBytes("UTF-8")))

  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)]

  protected def lastSnapshotAppliedHeight: Option[Int] = historyStorage.getIndex(LastSnapshotAppliedHeightKey)
    .map(w => Ints.fromByteArray(w.data))

  protected def pendingManifestOpt: Option[UtxoSnapshotManifest] = {
    historyStorage.getIndex(PendingManifestIdKey).flatMap {
      case r if r.data.nonEmpty =>
        historyStorage.modifierById(bytesToId(r.data)) match {
          case Some(m: UtxoSnapshotManifest) =>
            Some(m)
          case other =>
            assert(other.isEmpty, "History index is inconsistent")
            None
        }
      case _ =>
        None
    }
  }

  protected def pendingChunksQty: Int = historyStorage.getIndex(PendingChunksQtyKey)
    .map(w => Ints.fromByteArray(w.data))
    .getOrElse(0)

  def process(m: UtxoSnapshotChunk): ProgressInfo[ErgoPersistentModifier] = {
    pendingManifestOpt match {
      case Some(manifest: UtxoSnapshotManifest) =>
        lazy val lastHeaders = takeLastHeaders(manifest.blockId, Constants.LastHeadersInContext)
        val pendingChunks = pendingChunksQty
        if (pendingChunks == 1 && lastHeaders.nonEmpty) {
          // Time to apply snapshot
          val requiredChunks = manifest.chunkRoots.map(UtxoSnapshot.digestToId)
          val otherChunks = requiredChunks
            .map(historyStorage.modifierById)
            .collect { case Some(chunk: UtxoSnapshotChunk) => chunk }
          if (otherChunks.lengthCompare(requiredChunks.size - 1) == 0) {
            val snapshot = UtxoSnapshot(manifest, otherChunks :+ m, lastHeaders)
            val snapshotHeight = lastHeaders.head.height
            val indexesToInsert = Seq(
              LastSnapshotAppliedHeightKey -> ByteArrayWrapper(Ints.toByteArray(snapshotHeight)),
              PendingChunksQtyKey -> ByteArrayWrapper(Ints.toByteArray(0)),
              PendingManifestIdKey -> ByteArrayWrapper(Array.empty),
            )
            historyStorage.insert(Algos.idToBAW(m.id), indexesToInsert, Seq.empty)
            ProgressInfo(None, Seq.empty, Seq(snapshot), toDownload(lastHeaders.head))
          } else {
            log.warn(s"${requiredChunks.size - otherChunks.size} chunks are missed. Trying to request them again.")
            val chunksToRequest = requiredChunks
              .filterNot(_ == m.id)
              .diff(otherChunks.map(_.id))
              .map(UtxoSnapshotChunk.modifierTypeId -> _)
            val indexesToInsert = Seq(PendingChunksQtyKey -> ByteArrayWrapper(Ints.toByteArray(chunksToRequest.size)))
            historyStorage.insert(Algos.idToBAW(m.id), indexesToInsert, Seq.empty)
            ProgressInfo(None, Seq.empty, Seq.empty, chunksToRequest)
          }
        } else {
          val indexesToInsert = Seq(
            PendingChunksQtyKey -> ByteArrayWrapper(Ints.toByteArray(Math.max(0, pendingChunks - 1)))
          )
          historyStorage.insert(Algos.idToBAW(m.id), indexesToInsert, Seq(m))
          emptyProgressInfo
        }
      case _ =>
        emptyProgressInfo
    }
  }

  def validate(m: UtxoSnapshotChunk): Try[Unit] = {
    pendingManifestOpt match {
      case Some(manifest: UtxoSnapshotManifest) =>
        m.validate(manifest)
      case _ =>
        Failure(new Exception(s"Manifest for chunk ${m.id} is undefined"))
    }
  }

  private def takeLastHeaders(lastHeaderId: ModifierId, qty: Int): Seq[Header] = {
    (0 until qty).foldLeft(Seq.empty[Header]) { case (acc, _) =>
      historyStorage.modifierById(acc.headOption.map(_.parentId).getOrElse(lastHeaderId)) match {
        case Some(h: Header) =>
          acc :+ h
        case _ =>
          acc
      }
    }
  }

}
