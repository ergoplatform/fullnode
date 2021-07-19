package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.header.Header
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

case class ErgoSyncInfo(lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
    lastHeaderIds.map(b => Header.modifierTypeId -> b)
  }

  override type M = ErgoSyncInfo

  override lazy val serializer: ScorexSerializer[ErgoSyncInfo] = ErgoSyncInfoSerializer
}

object ErgoSyncInfo {
  // TODO move to config?
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends ScorexSerializer[ErgoSyncInfo] {

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    w.putUShort(obj.lastHeaderIds.size)
    obj.lastHeaderIds.foreach( id => w.putBytes(idToBytes(id)))
  }

  override def parse(r: Reader): ErgoSyncInfo = {
    val length = r.getUShort()
    require(length <= ErgoSyncInfo.MaxBlockIds + 1, "Too many block ids in sync info")
    val ids = (1 to length).map(_ => bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize)))
    ErgoSyncInfo(ids)
  }
}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer)
