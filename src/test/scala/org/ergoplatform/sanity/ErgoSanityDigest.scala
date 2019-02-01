package org.ergoplatform.sanity

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.nodeView.ErgoInterpreter
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.{WrappedDigestState, WrappedUtxoState}
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.{ErgoSettings, LaunchParameters}
import org.scalacheck.Gen
import scorex.core.idToBytes
import scorex.core.network.peer.PeerInfo
import scorex.core.network.{ConnectedPeer, Outgoing}
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContextExecutor

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST] {

  override val historyGen: Gen[HT] =
    generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedDigestState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, settings)).map { wus =>
      val digestState = DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, stateConstants)
      new WrappedDigestState(digestState, wus, settings, ErgoInterpreter(LaunchParameters))
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM = validFullBlock(None, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get

  override def totallyValidModifier(history: HT, state: DIGEST_ST): PM = {
    val parentOpt = history.bestHeaderOpt
    validFullBlock(parentOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState).header
  }

  override def totallyValidModifiers(history: HT, state: DIGEST_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val headerOpt = history.bestHeaderOpt
    (0 until count).foldLeft((headerOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(headerOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
      (Some(pm.header), acc._2 :+ pm)
    }._2.map(_.asInstanceOf[ErgoFullBlock].header)
  }

  override def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s = stateGen.sample.get
    val settings = ErgoSettings.read(None)
    val pool = ErgoMemPool.empty(settings)
    val v = h.openSurfaceIds().last
    s.store.update(ByteArrayWrapper(idToBytes(v)), Seq(), Seq())
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
        settings.scorexSettings.network,
        tp,
        h,
        pool)
    ))
    val m = totallyValidModifier(h, s)
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2

    val peerInfo = PeerInfo(
      0L,
      None,
      Some(""),
      Some(Outgoing),
      Seq.empty
    )
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = ConnectedPeer(
      inetSocketAddressGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
    (ref, h.syncInfo, m, tx, p, pchProbe, ncProbe, vhProbe, eventListener)
  }

  override def modifierWithTransactions(memoryPoolOpt: Option[MPool], customTransactionsOpt: Option[Seq[TX]]): CTM = {
    val boxHolder = boxesHolderGen.sample.get
    val txs = validTransactionsFromBoxHolder(boxHolder)._1
    val id = modifierIdGen.sample.get
    BlockTransactions(id, txs)
  }
}
