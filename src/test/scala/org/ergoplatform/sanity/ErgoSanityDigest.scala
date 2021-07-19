package org.ergoplatform.sanity

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.{WrappedDigestState, WrappedUtxoState}
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.idToBytes
import scorex.core.network.peer.PeerInfo
import scorex.core.network.ConnectedPeer
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContextExecutor

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST] {

  override val historyGen: Gen[HT] =
    generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedDigestState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, settings)).map { wus =>
      val digestState = DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
      new WrappedDigestState(digestState, wus, settings)
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM =
    validFullBlock(None, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get

  override def totallyValidModifier(history: HT, state: DIGEST_ST): PM = {
    val parentOpt = history.bestFullBlockOpt
    validFullBlock(parentOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState).header
  }

  override def totallyValidModifiers(history: HT, state: DIGEST_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val blockOpt = history.bestFullBlockOpt
    (0 until count).foldLeft((blockOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(blockOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
      (Some(pm), acc._2 :+ pm)
    }._2.map(_.asInstanceOf[ErgoFullBlock].header)
  }

  override def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, ScorexSerializer[PM]) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s = stateGen.sample.get
    val settings = ErgoSettings.read()
    val pool = ErgoMemPool.empty(settings)
    val v = h.openSurfaceIds().last
    s.store.update(idToBytes(v), Seq(), Seq())
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val tp = new NetworkTimeProvider(settings.scorexSettings.ntp)
    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val eventListener = TestProbe("EventListener")
    val ref = system.actorOf(Props(
      new SyncronizerMock(
        ncProbe.ref,
        vhProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        tp,
        h,
        pool)
    ))
    val m = totallyValidModifier(h, s)
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2


    val peerInfo = PeerInfo(defaultPeerSpec, Long.MaxValue)

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      lastMessage = 0,
      Some(peerInfo)
    )
    val serializer: ScorexSerializer[PM] = HeaderSerializer.asInstanceOf[ScorexSerializer[PM]]
    (ref, h.syncInfo, m, tx, p, pchProbe, ncProbe, vhProbe, eventListener, serializer)
  }

  override def modifierWithTransactions(memoryPoolOpt: Option[MPool], customTransactionsOpt: Option[Seq[TX]]): CTM = {
    val boxHolder = boxesHolderGen.sample.get
    val txs = validTransactionsFromBoxHolder(boxHolder)._1
    val id = modifierIdGen.sample.get
    BlockTransactions(id, 1: Byte, txs)
  }
}
