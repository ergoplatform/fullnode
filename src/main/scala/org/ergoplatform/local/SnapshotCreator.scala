package org.ergoplatform.local

import akka.actor.Actor
import org.ergoplatform.local.SnapshotCreator._
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.state.{UtxoSnapshot, UtxoSnapshotChunk, UtxoSnapshotManifest}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash.Digest32

class SnapshotCreator extends Actor {

  override def receive: Receive = {
    case TakeSnapshotOf(prover, lastHeader) =>
      val (manifest, chunks) = takeSnapshot(prover, lastHeader)
      val snapshot = UtxoSnapshot(manifest, chunks, Seq(lastHeader))
      sender() ! LocallyGeneratedModifier(snapshot)
  }
}

object SnapshotCreator {

  private implicit val hf: HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, HF]

  def takeSnapshot(prover: BatchAVLProver[Digest32, HF], lastHeader: Header): (UtxoSnapshotManifest, Seq[UtxoSnapshotChunk]) = {
    val (proverManifest, proverSubtrees) = serializer.slice(prover)
    val manifest = UtxoSnapshotManifest(proverManifest, lastHeader.id)
    val chunks = proverSubtrees.map(UtxoSnapshotChunk(_, manifest.id))
    manifest -> chunks
  }

  case class TakeSnapshotOf(prover: BatchAVLProver[Digest32, HF], lastHeader: Header)
}
