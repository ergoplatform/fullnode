package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.{Algos, CacheSettings}
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.{Failure, Success, Try}

/**
  * Storage for Ergo history
  *
  * @param indexStore   - Additional key-value storage for indexes, required by History for efficient work.
  *                     contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage(indexStore: Store, objectsStore: ObjectsStore, config: CacheSettings) extends ScorexLogging
  with AutoCloseable with ScorexEncoding {

  private val modifiersCache = CacheBuilder.newBuilder()
    .maximumSize(config.modifiersCacheSize)
    .build[String, ErgoPersistentModifier]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.indexesCacheSize)
    .build[ByteArrayWrapper, ByteArrayWrapper]

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    Option(modifiersCache.getIfPresent(id)) match {
      case Some(e) =>
        log.trace(s"Got modifier $id from cache")
        Some(e)
      case None =>
        objectsStore.get(id).flatMap { bBytes =>
          HistoryModifierSerializer.parseBytes(bBytes).recoverWith { case e =>
            log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bBytes)})")
            Failure(e)
          }.toOption match {
            case Some(pm) =>
              log.trace(s"Cache miss for existing modifier $id")
              modifiersCache.put(id, pm)
              Some(pm)
            case None => None
          }
        }
    }
  }

  def getIndex(id: ByteArrayWrapper): Option[ByteArrayWrapper] = Option(indexCache.getIfPresent(id)).orElse {
    indexStore.get(id).map { value =>
      indexCache.put(id, value)
      value
    }
  }

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(id)

  def contains(id: ModifierId): Boolean = objectsStore.contains(id)

  def insertObjects(objectsToInsert: Seq[ErgoPersistentModifier]): Try[Unit] = {
    objectsToInsert
      .map { o =>
        modifiersCache.put(o.id, o)
        objectsStore.put(o)
      }
      .foldLeft[Try[Unit]](Success(())) { case (acc, t) =>
        acc.flatMap(_ => t)
      }
  }

  def insert(id: ByteArrayWrapper,
             indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
             objectsToInsert: Seq[ErgoPersistentModifier]): Try[Unit] = Try {
    objectsToInsert.foreach { o =>
      modifiersCache.put(o.id, o)
      // TODO saving object to disc may be async here for performance reasons
      objectsStore.put(o).get
    }
    if (indexesToInsert.nonEmpty) {
      indexesToInsert.foreach(kv => indexCache.put(kv._1, kv._2))
      indexStore.update(
        id,
        Seq.empty,
        indexesToInsert)
    }
  }.recoverWith { case e =>
    log.error("Unable to perform insert", e)
    Failure(e)
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = idsToRemove.foreach { id =>
    modifiersCache.invalidate(id)
    objectsStore.delete(id)
  }

  override def close(): Unit = {
    log.warn("Closing history storage...")
    indexStore.close()
  }
}
