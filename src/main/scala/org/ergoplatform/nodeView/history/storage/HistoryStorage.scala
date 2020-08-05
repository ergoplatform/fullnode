package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.{Algos, CacheSettings}
import scorex.core.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Failure, Success}

/**
  * Storage for Ergo history
  *
  * @param indexStore   - Additional key-value storage for indexes, required by History for efficient work.
  *                     contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage(indexStore: LDBKVStore, objectsStore: LDBKVStore, config: CacheSettings)
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

  private val modifiersCache = CacheBuilder.newBuilder()
    .maximumSize(config.modifiersCacheSize)
    .build[ModifierId, ErgoPersistentModifier]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.indexesCacheSize)
    .build[ByteArrayWrapper, Array[Byte]]

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] =
    Option(modifiersCache.getIfPresent(id)) orElse
      objectsStore.get(idToBytes(id)).flatMap { bytes =>
        HistoryModifierSerializer.parseBytesTry(bytes) match {
          case Success(pm) =>
            log.trace(s"Cache miss for existing modifier $id")
            modifiersCache.put(id, pm)
            Some(pm)
          case Failure(_) =>
            log.warn(s"Failed to parse modifier ${id} from db (bytes are: ${Algos.encode(bytes)})")
            None
        }
      }

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]] =
    Option(indexCache.getIfPresent(id)).orElse {
      indexStore.get(id.data).map { value =>
        indexCache.put(id, value)
        value
      }
    }

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(idToBytes(id))

  def contains(id: ModifierId): Boolean = objectsStore.get(idToBytes(id)).isDefined

  def insert(indexesToInsert: Seq[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit = {
    objectsToInsert.foreach(o => modifiersCache.put(o.id, o))
    objectsStore.insert(
      objectsToInsert.map(m => idToBytes(m.id) -> HistoryModifierSerializer.toBytes(m))
    )
    if (indexesToInsert.nonEmpty) {
      indexesToInsert.foreach(kv => indexCache.put(kv._1, kv._2))
      indexStore.insert(indexesToInsert.map { case (k, v) => k.data -> v })
    }
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = {
    idsToRemove.foreach { id =>
      modifiersCache.invalidate(id)
    }
    objectsStore.remove(idsToRemove.map(idToBytes))
  }

  override def close(): Unit = {
    log.warn("Closing history storage...")
    indexStore.close()
    objectsStore.close()
  }

}
