package org.ergoplatform.mining

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import scorex.core.block.Block
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}

case class CandidateBlock(parentOpt: Option[Header],
                          version: Block.Version,
                          nBits: Long,
                          stateRoot: ADDigest,
                          adProofBytes: SerializedAdProof,
                          transactions: Seq[ErgoTransaction],
                          timestamp: Timestamp,
                          extension: ExtensionCandidate,
                          votes: Array[Byte]) {

  override def toString: String = s"CandidateBlock(${this.asJson})"

}

object CandidateBlock {

  implicit val jsonEncoder: Encoder[CandidateBlock] = (c: CandidateBlock) =>
    Map(
      "parentId" -> c.parentOpt.map(p => Algos.encode(p.id)).getOrElse("None").asJson,
      "version" -> c.version.asJson,
      "nBits" -> c.nBits.asJson,
      "stateRoot" -> Algos.encode(c.stateRoot).asJson,
      "adProofBytes" -> Algos.encode(c.adProofBytes).asJson,
      "timestamp" -> c.timestamp.asJson,
      "transactions" -> c.transactions.map(_.asJson).asJson,
      "transactionsNumber" -> c.transactions.length.asJson,
      "votes" -> Algos.encode(c.votes).asJson,
      "extensionHash" -> Algos.encode(c.extension.digest).asJson
    ).asJson

}
