package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotManifest}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.reflect.ClassTag
import scala.util.{Success, Try}

/**
  * Contains all functions for locally generated state snapshot processing.
  */
trait UtxoSnapshotProcessor extends ScorexLogging with ScorexEncoding {

  protected val config: NodeConfigurationSettings

  protected val historyStorage: HistoryStorage

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T]

  def process(m: UtxoSnapshot): ProgressInfo[ErgoPersistentModifier] = {
    m.lastHeaders.headOption.foreach(h => pruneOldSnapshots(h.height))
    log.info(s"Appending new state snapshot ${m.id}")
    historyStorage.insertObjects(m.chunks :+ m.manifest)
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  def pruneOldSnapshots(newHeight: Int): Unit = {
    assert(newHeight % config.snapshotCreationInterval == 0, "Trying to prune snapshots at wrong height.")
    val heightToRemove = newHeight - config.snapshotCreationInterval * config.keepLastSnapshots
    val outdatedIds = headerIdsAtHeight(heightToRemove)
      .map { id =>
        typedModifierById[Header](id).flatMap { h =>
          val manifestId = UtxoSnapshot.digestToId(h.stateRoot)
          typedModifierById[UtxoSnapshotManifest](manifestId).map { manifest =>
            val chunks = manifest.chunkRoots.map(UtxoSnapshot.digestToId)
            manifestId -> chunks
          }
        }
      }
      .collect { case Some(value) => value }
      .foldLeft(Seq.empty[ModifierId]) { case (acc, (manifest, chunks)) =>
        acc ++ chunks :+ manifest
      }
    log.info(s"Removing old snapshot $manifest")
    historyStorage.remove(outdatedIds)
  }

  // UtxoSnapshot could be generated locally only, so it does not require validation.
  def validate(m: UtxoSnapshot): Try[Unit] = Success(())

}
