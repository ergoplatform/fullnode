package org.ergoplatform.sanity

import akka.actor.ActorRef
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.Constants
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.utils.{ErgoTestHelpers, HistoryTestHelpers}
import org.scalacheck.Gen
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{PersistentNodeViewModifier, bytesToId, idToBytes}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.testkit.generators.{ModifierProducerTemplateItem, SynInvalid, Valid}
import scorex.testkit.properties._
import scorex.testkit.properties.mempool.{MempoolRemovalTest, MempoolTransactionsTest}
import scorex.testkit.properties.state.StateApplicationTest
import scorex.utils.Random

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait ErgoSanity[ST <: MinimalState[PM, ST]] extends HistoryTests[TX, PM, SI, HT]
  with StateApplicationTest[PM, ST]
  //with WalletSecretsTest[P, TX, PM]
  with MempoolTransactionsTest[TX, MPool]
  with MempoolRemovalTest[TX, MPool, PM, CTM, HT, SI]
  //with BoxStateChangesGenerationTest[P, TX, PM, B, ST]
  with NodeViewSynchronizerTests[TX, PM, ST, SI, HT, MPool]
  with ErgoTestHelpers
  with HistoryTestHelpers {


  override val memPool: MPool = ErgoMemPool.empty(settings)

  //Generators
  override lazy val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
  override lazy val memPoolGenerator: Gen[MPool] = emptyMemPoolGen

  override def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.CurrentVersion,
      settings.chainSettings.initialNBits,
      ADDigest @@ Array.fill(HashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Math.max(timeProvider.time(), bestTimestamp),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get
  }

  override def syntacticallyInvalidModifier(history: HT): PM =
    syntacticallyValidModifier(history).copy(parentId = bytesToId(Random.randomBytes(32)))

  private val hf = Blake2b256

  def makeSyntacticallyInvalid(mod: PM): PM = mod match {
    case fb: ErgoFullBlock =>
      val parentId = fb.header.parentId
      val header = fb.header.copy(parentId = bytesToId(hf(idToBytes(parentId))))
      fb.copy(header = header)
    case h: Header => h.copy(parentId = bytesToId(hf(idToBytes(h.parentId))))
    case v => v
  }

  def customModifiers(history: HT,
                      state: ST,
                      template: Seq[ModifierProducerTemplateItem]): Seq[PM] =
    template.zip(totallyValidModifiers(history, state, template.length))
      .map { case (templateItem, mod) =>
        templateItem match {
          case Valid => mod
          case SynInvalid => makeSyntacticallyInvalid(mod)
        }
      }

  class SyncronizerMock(networkControllerRef: ActorRef,
                        viewHolderRef: ActorRef,
                        syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                        networkSettings: NetworkSettings,
                        timeProvider: NetworkTimeProvider,
                        history: ErgoHistory,
                        pool: ErgoMemPool)
                       (implicit ec: ExecutionContext) extends ErgoNodeViewSynchronizer(
    networkControllerRef,
    viewHolderRef,
    syncInfoSpec,
    networkSettings,
    timeProvider)(ec) {

    override def preStart(): Unit = {
      this.historyReaderOpt = Some(history)
      this.mempoolReaderOpt = Some(pool)
      super.preStart()
    }

    override protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
      mod match {
        case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) =>
          fb.toSeq.foreach(s => broadcastModifierInv(s))
        case h: Header if h.isNew(timeProvider, 1.hour) =>
          broadcastModifierInv(h)
        case _ =>
      }
    }
  }

}

object ErgoSanity {
  type TX = ErgoTransaction
  type B = ErgoBox
  type PM = ErgoPersistentModifier
  type CTM = BlockTransactions
  type SI = ErgoSyncInfo
  type HT = ErgoHistory
  type UTXO_ST = UtxoState
  type DIGEST_ST = DigestState
  type MPool = ErgoMemPool

}
