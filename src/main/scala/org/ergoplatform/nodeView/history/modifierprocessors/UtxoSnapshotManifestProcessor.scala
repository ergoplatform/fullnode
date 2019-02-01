package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Algos
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ScorexLogging, idToBytes}

import scala.util.{Failure, Try}

trait UtxoSnapshotManifestProcessor extends ScorexLogging with ScorexEncoding {

  protected val historyStorage: HistoryStorage

  protected val PendingChunksQtyKey: ByteArrayWrapper

  protected val PendingManifestIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("pending-manifest".getBytes("UTF-8")))

  protected def pendingManifestOpt: Option[UtxoSnapshotManifest]

  def process(m: UtxoSnapshotManifest): ProgressInfo[ErgoPersistentModifier] = {
    val chunksToRequest = m.chunkRoots.map(UtxoSnapshot.digestToId).map(UtxoSnapshotChunk.modifierTypeId -> _)
    val indexesToInsert = Seq(
      PendingManifestIdKey -> ByteArrayWrapper(idToBytes(m.id)),
      PendingChunksQtyKey -> ByteArrayWrapper(Ints.toByteArray(chunksToRequest.size))
    )
    historyStorage.insert(Algos.idToBAW(m.id), indexesToInsert, Seq(m))
    ProgressInfo(None, Seq.empty, Seq.empty, chunksToRequest)
  }

  def validate(m: UtxoSnapshotManifest): Try[Unit] = historyStorage.modifierById(m.blockId) match {
    case Some(h: Header) if pendingManifestOpt.isEmpty => m.validate(h)
    case Some(_) => Failure(new Exception("Another snapshot isn't processed yet"))
    case _ => Failure(new Exception("Header manifest relates to is not found in history"))
  }

}
