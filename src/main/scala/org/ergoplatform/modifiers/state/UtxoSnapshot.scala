package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

/**
  * Holds all required elements to restore consistent state from snapshot.
  */
case class UtxoSnapshot(manifest: UtxoSnapshotManifest,
                        chunks: Seq[UtxoSnapshotChunk],
                        lastHeaders: Seq[Header])
  extends ErgoPersistentModifier {

  override type M = UtxoSnapshot

  override lazy val id: ModifierId = manifest.id

  override val modifierTypeId: ModifierTypeId = UtxoSnapshot.modifierTypeId

  override def serializedId: Array[Byte] = manifest.serializedId

  override val sizeOpt: Option[Int] = None

  override lazy val serializer: Serializer[M] =
    throw new Exception("Serialization for UtxoSnapshot is not supported")

  override def parentId: ModifierId = manifest.blockId

}

object UtxoSnapshot {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (108: Byte)

  def digestToSerializedId(digest: Array[Byte]): Digest32 = Algos.hash(digest)

  def digestToId(digest: Array[Byte]): ModifierId = bytesToId(digestToSerializedId(digest))

}
