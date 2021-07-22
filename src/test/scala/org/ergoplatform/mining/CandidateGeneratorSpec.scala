package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.{StatusReply, ask}
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, Input}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput

import scala.concurrent.duration._

class CandidateGeneratorSpec extends AnyFlatSpec with ErgoTestHelpers with Eventually {

  implicit private val timeout: Timeout = defaultTimeout

  private val newBlockSignal: Class[SemanticallySuccessfulModifier[_]] = classOf[SemanticallySuccessfulModifier[_]]
  private val newBlockDelay: FiniteDuration = 30.seconds
  private val candidateGenDelay: FiniteDuration = 3.seconds
  private val blockValidationDelay: FiniteDuration = 2.seconds

  val defaultSettings: ErgoSettings = {
    val empty = ErgoSettings.read()
    val nodeSettings = empty.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = 1.second,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  it should "provider candidate to internal miner and verify and apply his solution" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, timeProvider, defaultSettings)
    ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)

    // after applying solution from miner
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    system.terminate()
  }

  it should "let multiple miners compete" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, timeProvider, defaultSettings)

    val m1 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)
    val m2 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)
    val m3 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)

    // after applying solution from miner
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    m1.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)
    val m1Count = testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])
    m2.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)
    val m2Count = testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])
    m3.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)
    val m3Count = testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])

    List(m1Count, m2Count, m3Count).map(_.count).sum shouldBe 3
    system.terminate()
  }

  it should "cache candidate until newly mined block is applied" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, timeProvider, defaultSettings)

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)

    val block = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        defaultSettings.chainSettings.powScheme.proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000).get
    }

    // now block should be cached
    (0 to 20).foreach { _ =>
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
      testProbe.expectMsgClass(2.millis, classOf[StatusReply[_]])
    }

    candidateGenerator.tell(block.header.powSolution, testProbe.ref)
    testProbe.expectMsg(blockValidationDelay, StatusReply.success(()))
    // after applying solution
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    system.terminate()
  }

  it should "pool transactions should be removed from pool when block is mined" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)
    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, timeProvider, defaultSettings)

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val history: ErgoHistoryReader = readers.h
    val startBlock: Option[Header] = history.bestHeaderOpt

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme.proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000).get
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
    }
    testProbe.expectMsg(blockValidationDelay, StatusReply.success(()))
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal) // new block applied

    // build new transaction that uses miner's reward as input
    val prop: DLogProtocol.ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray("test".getBytes())).publicImage
    val newlyMinedBlock = readers.h.bestFullBlockOpt.get
    val rewardBox: ErgoBox = newlyMinedBlock.transactions.last.outputs.last
    rewardBox.propositionBytes shouldBe ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk).bytes
    val input = Input(rewardBox.id, emptyProverResult)
    val outputs = IndexedSeq(new ErgoBoxCandidate(rewardBox.value, prop, readers.s.stateContext.currentHeight))
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)
    val tx = ErgoTransaction(defaultProver.sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext).get)

    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx), reply = true), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme.proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000).get
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
    }
    testProbe.expectMsg(blockValidationDelay, StatusReply.success(()))
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    // new transaction should be cleared from pool after applying new block
    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0

    // validate total amount of transactions created
    val blocks: IndexedSeq[ErgoFullBlock] = readers.h.chainToHeader(startBlock, readers.h.bestHeaderOpt.get)._2.headers.flatMap(readers.h.getFullBlock)
    val txs: Seq[ErgoTransaction] = blocks.flatMap(_.blockTransactions.transactions)
    txs should have length 3 // 2 rewards and one regular tx
    system.terminate()
  }

  it should "multiple miners should should compete for mining a block" in new TestKit(ActorSystem()) {


  }
}
