package org.ergoplatform.settings

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.modifiers.state.{UtxoSnapshotChunk, UtxoSnapshotChunkSerializer, UtxoSnapshotManifest, UtxoSnapshotManifestSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.{ModifierTypeId, NodeViewModifier}

object Constants {

  val HashLength: Int = 32

  val CoinsInOneErgo: Long = 1000000000

  val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))
  val InitialDifficulty: Difficulty = BigInt(1)
  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = HashLength

  val BlocksPerHour = 30

  val BlocksPerDay: Int = BlocksPerHour * 24

  val BlocksPerWeek: Int = BlocksPerDay * 7

  val BlocksPerMonth: Int = BlocksPerDay * 30

  val BlocksPerYear: Int = BlocksPerDay * 365

  //For how many blocks a box could be put into the state with no paying.
  //4 years
  val StoragePeriod: Int = 4 * BlocksPerYear

  val StorageContractCost: Long = 50

  val StorageIndexVarId: Byte = Byte.MaxValue

  // Number of last block headers available is scripts from ErgoStateContext
  val LastHeadersInContext = 10

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    Header.modifierTypeId -> HeaderSerializer,
    Extension.modifierTypeId -> ExtensionSerializer,
    BlockTransactions.modifierTypeId -> BlockTransactionsSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> ErgoTransactionSerializer,
    UtxoSnapshotManifest.modifierTypeId -> UtxoSnapshotManifestSerializer,
    UtxoSnapshotChunk.modifierTypeId -> UtxoSnapshotChunkSerializer
  )

  val SoftForkEpochs = 32 //about 45.5 days

  val extensionMaxSize: Int = 10 * 1024 //10 kb
}
