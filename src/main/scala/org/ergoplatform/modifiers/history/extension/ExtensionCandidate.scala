package org.ergoplatform.modifiers.history.extension

import org.ergoplatform.settings.Algos
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

/**
  * Extension block section with not filled header id
  */
class ExtensionCandidate(val fields: Seq[(Array[Byte], Array[Byte])]) {
  lazy val merkleTree: MerkleTree[Digest32] = Extension.merkleTree(fields)

  lazy val digest: Digest32 = Algos.merkleTreeRoot(merkleTree)

  def toExtension(headerId: ModifierId): Extension = Extension(headerId, fields)

  def ++(that: ExtensionCandidate): ExtensionCandidate = ExtensionCandidate(fields ++ that.fields)

  def proofFor(key: Array[Byte]): Option[MerkleProof[Digest32]] =
    fields.find(_._1 sameElements key)
      .map(Extension.kvToLeaf)
      .flatMap(kv => merkleTree.proofByElement(Leaf[Digest32](LeafData @@ kv)(Algos.hash)))
}

object ExtensionCandidate {
  def apply(fields: Seq[(Array[Byte], Array[Byte])]): ExtensionCandidate = new ExtensionCandidate(fields)
}
