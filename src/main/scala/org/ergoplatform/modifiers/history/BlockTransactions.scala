package org.ergoplatform.modifiers.history

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.block.Block.Version
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.Extensions._


/**
  * Section of a block which contains transactions.
  *
  * @param headerId     - identifier of a header of a corresponding block
  * @param blockVersion - protocol version for the block
  * @param txs          - transactions of the block
  * @param sizeOpt      - (optional) size of the section (cached to not be calculated again)
  */
case class BlockTransactions(headerId: ModifierId,
                             blockVersion: Version,
                             txs: Seq[ErgoTransaction],
                             override val sizeOpt: Option[Int] = None)
  extends BlockSection with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  assert(txs.nonEmpty, "Block should always contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = BlockTransactions.modifierTypeId

  /**
    * Ids of block transactions
    */
  lazy val txIds: Seq[Array[Byte]] = txs.map(_.serializedId)

  /**
    * Ids of transaction witnesses (signatures aka spending proofs).
    */
  lazy val witnessIds: Seq[Array[Byte]] = txs.map(tx => tx.witnessSerializedId)

  /**
    * Non-empty (because there's at least 1 transaction) Merkle tree of the block transactions
    */
  lazy val merkleTree: MerkleTree[Digest32] = if (blockVersion == Header.InitialVersion) {
    Algos.merkleTree(LeafData @@ txIds)
  } else {
    Algos.merkleTree(LeafData @@ (txIds ++ witnessIds))
  }

  /**
    * Root hash of the Merkle tree of block transactions
    */
  override lazy val digest: Digest32 = merkleTree.rootHash

  /**
    * Calculates Merkle-tree based membership proof for a given transaction identifier
    *
    * @param txId - transaction identifier
    * @return Some(proof) or None (if transaction with given id is not in the block)
    */
  def proofFor(txId: Array[Byte]): Option[MerkleProof[Digest32]] =
    merkleTree.proofByElement(Leaf[Digest32](LeafData @@ txId)(Algos.hash))

  def proofFor(txId: ModifierId): Option[MerkleProof[Digest32]] = proofFor(scorex.util.idToBytes(txId))

  override type M = BlockTransactions

  override lazy val serializer: ScorexSerializer[BlockTransactions] = BlockTransactionsSerializer

  override def toString: String = {
    val idStr = Algos.encode(id)
    val headerIdStr = Algos.encode(headerId)
    val displayMaxObjects = 5
    // Artificial limit to show only first `displayMaxObjects` txs.
    val txsStr = txs.take(displayMaxObjects).map(_.toString).mkString(",")
    val txsSuffix = if (txs.lengthCompare(displayMaxObjects) > 0) ", ..." else ""

    s"BlockTransactions(id: $idStr, headerId: $headerIdStr, txs: $txsStr$txsSuffix)"
  }

  override lazy val transactions: Seq[ErgoTransaction] = txs
}

object BlockTransactions extends ApiCodecs {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  // Used in the miner when a BlockTransaction instance is not generated yet (because a header is not known)
  def transactionsRoot(txs: Seq[ErgoTransaction], blockVersion: Version): Digest32 = {
    if (blockVersion == Header.InitialVersion) {
      Algos.merkleTreeRoot(LeafData @@ txs.map(_.serializedId))
    } else {
      Algos.merkleTreeRoot(LeafData @@ (txs.map(_.serializedId) ++ txs.map(_.witnessSerializedId)))
    }
  }

  // Could be useful when only digest of transactions is available, not a BlockTransaction instance
  def proofValid(transactionsDigest: Digest32, proof: MerkleProof[Digest32]): Boolean = proof.valid(transactionsDigest)

  def proofValid(transactionsDigest: Digest32, proof: TransactionMembershipProof): Boolean =
    proofValid(transactionsDigest, proof.proof)

  implicit val jsonEncoder: Encoder[BlockTransactions] = { bt: BlockTransactions =>
    Map(
      "headerId" -> Algos.encode(bt.headerId).asJson,
      "transactions" -> bt.txs.map(_.asJson).asJson,
      "blockVersion" -> bt.blockVersion.asJson,
      "size" -> bt.size.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[BlockTransactions] = { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      transactions <- c.downField("transactions").as[List[ErgoTransaction]]
      blockVersion <- c.downField("blockVersion").as[Version]
      size <- c.downField("size").as[Int]
    } yield BlockTransactions(headerId, blockVersion, transactions, Some(size))
  }
}

object BlockTransactionsSerializer extends ScorexSerializer[BlockTransactions] {
  // See a comment in the parse() function
  val MaxTransactionsInBlock = 10000000

  override def serialize(bt: BlockTransactions, w: Writer): Unit = {
    w.putBytes(idToBytes(bt.headerId))
    if (bt.blockVersion > 1) {
      w.putUInt(MaxTransactionsInBlock + bt.blockVersion)
    }
    w.putUInt(bt.txs.size)
    bt.txs.foreach { tx =>
      ErgoTransactionSerializer.serialize(tx, w)
    }
  }

  override def parse(r: Reader): BlockTransactions = {
    val startPos = r.position
    val headerId: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val verOrCount = r.getUInt().toIntExact

    /**
      * A hack to avoid need for a database rescan if older version of the serializer was used to put.
      * block transactions into.
      *
      * We consider that in a block there could be no more than 10,000,000 transactions.
      *
      * Then the new serializer puts 10,000,000 + block version (while the old one just puts tx count with no version),
      * and the reader knows that a new serializer was used if the first unsigned integer read is more than 10,000,000.
      */
    var blockVersion = 1: Byte
    var txCount = verOrCount
    if (verOrCount > MaxTransactionsInBlock) {
      blockVersion = (verOrCount - MaxTransactionsInBlock).toByte
      txCount = r.getUInt().toIntExact
    }

    val txs = (1 to txCount).map { _ =>
      ErgoTransactionSerializer.parse(r)
    }
    BlockTransactions(headerId, blockVersion, txs, Some(r.position - startPos))
  }
}
