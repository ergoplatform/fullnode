package org.ergoplatform.modifiers.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.annotation.tailrec
import scala.util.Try

/**
  * Holds prover manifest and id of block snapshot relates to.
  */
case class UtxoSnapshotManifest(proverManifest: BatchAVLProverManifest[Digest32, Algos.HF],
                                blockId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = UtxoSnapshotManifest

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotManifest.modifierTypeId

  override lazy val serializer: Serializer[UtxoSnapshotManifest] = UtxoSnapshotManifestSerializer

  override lazy val sizeOpt: Option[Int] = None

  lazy val rootDigest: ADDigest = {
    val (root, height) = proverManifest.oldRootAndHeight
    ADDigest @@ (root.label :+ height.toByte)
  }

  override def serializedId: Array[Byte] = UtxoSnapshot.digestToSerializedId(rootDigest)

  override def parentId: ModifierId = blockId

  def chunkRoots: Seq[ADDigest] = {
    @tailrec
    def proxyNodes(nodes: Seq[ProverNodes[Digest32]], acc: Seq[ADDigest] = Seq.empty): Seq[ADDigest] = {
      nodes match {
        case (n: ProxyInternalNode[Digest32]) +: tail =>
          proxyNodes(tail, acc ++ Seq(n.leftLabel, n.rightLabel).map(ADDigest !@@ _))
        case (n: InternalProverNode[Digest32]) +: tail =>
          proxyNodes(n.left +: n.right +: tail, acc)
        case (_: ProverLeaf[Digest32]) +: tail =>
          proxyNodes(tail, acc)
        case seq if seq.isEmpty =>
          acc
      }
    }
    proxyNodes(Seq(proverManifest.oldRootAndHeight._1))
  }

  def validate(header: Header): Try[Unit] = {
    failFast
      .demandEqualIds(blockId, header.id, s"`blockId` does not correspond to $header")
      .demandEqualArrays(rootDigest, header.stateRoot, "`rootDigest` does not correspond to header's `stateRoot`")
      .demand(proverManifest.keyLength == Constants.HashLength, "Invalid key length declared")
      .demand(proverManifest.valueLengthOpt.isEmpty, "Invalid value length declared")
      .demand(validManifestTree, "Manifest tree is invalid")
      .result
      .toTry
  }

  /**
    * Checks that manifest tree consists of valid proxy nodes.
    */
  private def validManifestTree: Boolean = {
    @tailrec
    def validationLoop(nodes: Seq[(ProverNodes[Digest32], Int)], maxHeight: Int = 0): Boolean = {
      nodes match {
        case (n: ProxyInternalNode[Digest32], parentHeight) +: tail =>
          if (n.isEmpty && java.util.Arrays.equals(n.computeLabel, n.label)) {
            validationLoop(tail, math.max(maxHeight, parentHeight + 1))
          } else {
            false
          }
        case (n: InternalProverNode[Digest32], parentHeight) +: tail
          if Option(n.left).flatMap(_ => Option(n.right)).nonEmpty =>
          val childHeight = parentHeight + 1
          validationLoop((n.left, childHeight) +: (n.right, childHeight) +: tail)
        case (_: ProverLeaf[Digest32], parentHeight) +: tail =>
          validationLoop(tail, math.max(maxHeight, parentHeight + 1))
        case seq if seq.isEmpty =>
          maxHeight >= (proverManifest.oldRootAndHeight._2 / 2)
        case _ =>
          false
      }
    }
    validationLoop(Seq(proverManifest.oldRootAndHeight._1 -> 1))
  }

}

object UtxoSnapshotManifest {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)
}

object UtxoSnapshotManifestSerializer extends Serializer[UtxoSnapshotManifest] {

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotManifest): Array[Byte] = {
    val serializedProverManifest = serializer.manifestToBytes(obj.proverManifest)
    Bytes.concat(
      idToBytes(obj.blockId),
      Ints.toByteArray(serializedProverManifest.length),
      serializedProverManifest
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val proverManifestLen = Ints.fromByteArray(
      bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    val requiredBytesLen = Constants.ModifierIdSize + 4 + proverManifestLen
    val proverManifestTry = serializer.manifestFromBytes(
      bytes.slice(Constants.ModifierIdSize + 4, requiredBytesLen))
    proverManifestTry.map(UtxoSnapshotManifest(_, blockId))
  }.flatten

}
