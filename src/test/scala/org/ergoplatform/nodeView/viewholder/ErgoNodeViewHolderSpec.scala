package org.ergoplatform.nodeView.viewholder

import java.io.File

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, FileUtils, NodeViewTestConfig, NodeViewTestOps, TestCase}
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.testkit.utils.NoShrink
import scorex.util.{ModifierId, bytesToId}

class ErgoNodeViewHolderSpec extends ErgoPropertyTest with NodeViewTestOps with NoShrink {

  private val t1 = TestCase("check genesis state") { fixture =>
    import fixture._
    getCurrentState.rootHash shouldBe getGenesisStateDigest
  }

  private val t2 = TestCase("check history after genesis") { fixture =>
    import fixture._
    getBestHeaderOpt shouldBe None
  }

  private val t3 = TestCase("apply valid block header") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val block = validFullBlock(None, us, bh)

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    getHistoryHeight shouldBe ErgoHistory.GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(ErgoHistory.GenesisHeight)
    getLastHeadersLength(10) shouldBe 1
    getOpenSurfaces shouldBe Seq(block.header.id)
    getBestHeaderOpt shouldBe Some(block.header)
  }

  private val t4 = TestCase("apply valid block as genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    nodeViewHolderRef ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    if (verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.adProofs.value)
      expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.extension)
      expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      getBestFullBlockOpt shouldBe Some(genesis)
    }
  }

  private val t5 = TestCase("apply full blocks after genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    applyBlock(genesis) shouldBe 'success

    val block = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(block) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockOpt shouldBe Some(block)
    }

    getBestHeaderOpt shouldBe Some(block.header)
    getHistoryHeight shouldBe block.header.height
    getLastHeadersLength(10) shouldBe 2
  }

  private val t6 = TestCase("add transaction to memory pool") { fixture =>
    import fixture._
    if (stateType == Utxo) {
      val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      val boxes = ErgoState.boxChanges(genesis.transactions)._2.find(_.ergoTree == Constants.TrueLeaf)
      boxes.nonEmpty shouldBe true

      val tx = validTransactionFromBoxes(boxes.toIndexedSeq)
      subscribeEvents(classOf[FailedTransaction])
      nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      expectNoMsg()
      getPoolSize shouldBe 1
    }
  }

  private val t7 = TestCase("apply statefuly invalid full block") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    // TODO looks like another bug is still present here, see https://github.com/ergoplatform/ergo/issues/309
    if (verifyTransactions) {
      applyBlock(genesis) shouldBe 'success

      val block = validFullBlock(Some(genesis), wusAfterGenesis)
      val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

      applyBlock(block) shouldBe 'success
      getBestHeaderOpt shouldBe Some(block.header)
      if (verifyTransactions) {
        getRootHash shouldBe Algos.encode(wusAfterBlock.rootHash)
      }
      getBestHeaderOpt shouldBe Some(block.header)

      val brokenBlock = generateInvalidFullBlock(Some(block), wusAfterBlock)
      applyBlock(brokenBlock) shouldBe 'success

      val brokenBlock2 = generateInvalidFullBlock(Some(block), wusAfterBlock)
      brokenBlock2.header should not be brokenBlock.header
      applyBlock(brokenBlock2) shouldBe 'success

      getBestFullBlockOpt shouldBe Some(block)
      getRootHash shouldBe Algos.encode(wusAfterBlock.rootHash)
      getBestHeaderOpt shouldBe Some(block.header)
    }
  }

  /**
    * Generates statefuly invalid full block (contains invalid transactions).
    */
  private def generateInvalidFullBlock(parentBlockOpt: Option[ErgoFullBlock], parentState: WrappedUtxoState) = {
    val validInterlinks = popowAlgos.updateInterlinks(parentBlockOpt.map(_.header), parentBlockOpt.map(_.extension))
    val extensionIn = popowAlgos.interlinksToExtension(validInterlinks).toExtension(modifierIdGen.sample.get)
    val brokenBlockIn = validFullBlock(parentBlockOpt, parentState)
    val headTx = brokenBlockIn.blockTransactions.txs.head
    val wrongBoxId: ADKey = ADKey !@@ Algos.hash("wrong input")
    val newInput = headTx.inputs.head.copy(boxId = wrongBoxId)
    val brokenTransactionsIn = brokenBlockIn.blockTransactions
      .copy(txs = headTx.copy(inputs = newInput +: headTx.inputs.tail) +: brokenBlockIn.blockTransactions.txs.tail)
    val brokenHeader = brokenBlockIn.header
      .copy(transactionsRoot = brokenTransactionsIn.digest, extensionRoot = extensionIn.digest)
    val brokenTransactions = brokenTransactionsIn.copy(headerId = brokenHeader.id)
    val brokenProofs = brokenBlockIn.adProofs.value.copy(headerId = brokenHeader.id)
    val extension = extensionIn.copy(headerId = brokenHeader.id)
    ErgoFullBlock(brokenHeader, brokenTransactions, extension, Some(brokenProofs))
  }

  private val t8 = TestCase("switching for a better chain") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootHash)

    val chain1block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    val expectedBestFullBlockOpt = if (verifyTransactions) Some(chain1block1) else None
    applyBlock(chain1block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val chain2block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(chain2block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)
    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    applyBlock(chain2block2) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockEncodedId shouldBe Some(chain2block2.header.encodedId)
    }

    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getRootHash shouldBe Algos.encode(chain2block2.header.stateRoot)
  }

  private val t9 = TestCase("UTXO state should generate adProofs and put them in history") { fixture =>
    import fixture._
    if (stateType == StateType.Utxo) {
      val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)

      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.header)
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.extension)

      getBestFullBlockOpt shouldBe Some(genesis)
      getModifierById(genesis.adProofs.value.id) shouldBe genesis.adProofs
    }
  }

  private val t10 = TestCase("NodeViewHolder start from inconsistent state") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    applyBlock(genesis) shouldBe 'success

    val block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(block1) shouldBe 'success
    getBestFullBlockOpt shouldBe Some(block1)
    getRootHash shouldBe Algos.encode(block1.header.stateRoot)

    stopNodeViewHolder()
    val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
    FileUtils.deleteRecursive(stateDir)
    startNodeViewHolder()

    getRootHash shouldBe Algos.encode(block1.header.stateRoot)
  }

  private val t11 = TestCase("apply payload in incorrect order (excluding extension)") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootHash)

    val chain2block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[Header]])
    nodeViewHolderRef ! LocallyGeneratedModifier(chain2block1.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    applyBlock(chain2block2, excludeExt = true) shouldBe 'success
    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getBestFullBlockEncodedId shouldBe Some(genesis.header.encodedId)

    applyPayload(chain2block1, excludeExt = true) shouldBe 'success
    getBestHeaderEncodedId shouldBe Some(chain2block2.header.encodedId)
  }

  private val t12 = TestCase("Do not apply txs with wrong header id") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val block = validFullBlock(None, us, bh)
    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]
    val currentHeight = getHistoryHeight
    currentHeight shouldBe ErgoHistory.GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(ErgoHistory.GenesisHeight)

    val randomId = modifierIdGen.sample.value
    val wrongTxs1 = block.blockTransactions.copy(headerId = randomId)
    val wrongTxs2 = {
      val txs = block.blockTransactions.transactions
      val tx = txs.head
      val wrongOutputs = tx.outputCandidates.map(o =>
        new ErgoBoxCandidate(o.value + 10L, o.ergoTree, currentHeight, o.additionalTokens, o.additionalRegisters)
      )
      val wrongTxs = tx.copy(outputCandidates = wrongOutputs) +: txs.tail
      block.blockTransactions.copy(txs = wrongTxs)
    }
    val wrongTxs3 = {
      val txs = block.blockTransactions.transactions
      val tx = txs.head
      val wrongInputs = tx.inputs.map { input =>
        input.copy(boxId = ADKey @@ input.boxId.reverse)
      }
      val wrongTxs = tx.copy(inputs = wrongInputs) +: txs.tail
      block.blockTransactions.copy(txs = wrongTxs)
    }

    nodeViewHolderRef ! LocallyGeneratedModifier[BlockTransactions](wrongTxs1)
    expectMsgType[SyntacticallyFailedModification[BlockTransactions]]

    nodeViewHolderRef ! LocallyGeneratedModifier[BlockTransactions](wrongTxs2)
    expectMsgType[SyntacticallyFailedModification[BlockTransactions]]

    nodeViewHolderRef ! LocallyGeneratedModifier[BlockTransactions](wrongTxs3)
    expectMsgType[SyntacticallyFailedModification[BlockTransactions]]

    nodeViewHolderRef ! LocallyGeneratedModifier[BlockTransactions](block.blockTransactions)
    expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
  }

  private val t13 = TestCase("Do not apply wrong adProofs") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val block = validFullBlock(None, us, bh)
    getBestHeaderOpt shouldBe None

    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    val randomId = modifierIdGen.sample.value
    val wrongProofsBytes = SerializedAdProof @@ block.adProofs.value.proofBytes.reverse
    val wrongProofs1 = block.adProofs.map(_.copy(headerId = randomId))
    val wrongProofs2 = block.adProofs.map(_.copy(proofBytes = wrongProofsBytes))

    nodeViewHolderRef ! LocallyGeneratedModifier[ADProofs](wrongProofs1.value)
    expectMsgType[SyntacticallyFailedModification[ADProofs]]
    nodeViewHolderRef ! LocallyGeneratedModifier[ADProofs](wrongProofs2.value)
    expectMsgType[SyntacticallyFailedModification[ADProofs]]

    nodeViewHolderRef ! LocallyGeneratedModifier[ADProofs](block.adProofs.value)
    expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
  }

  private val t14 = TestCase("do not apply genesis block header if " +
    "it's not equal to genesisId from config") { fixture =>
    import fixture._
    updateConfig(genesisIdConfig(modifierIdGen.sample))
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val block = validFullBlock(None, us, bh)

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallyFailedModification[Header]]
    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight
  }

  private val t15 = TestCase("apply genesis block header if it's equal to genesisId from config") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val block = validFullBlock(None, us, bh)
    updateConfig(genesisIdConfig(Some(block.header.id)))

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    nodeViewHolderRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]
    getHistoryHeight shouldBe ErgoHistory.GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(ErgoHistory.GenesisHeight)
  }

  private val t16 = TestCase("apply forks that include genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val wusGenesis = WrappedUtxoState(us, bh, stateConstants)


    val chain1block1 = validFullBlock(parentOpt = None, us, bh)
    val expectedBestFullBlockOpt = if (verifyTransactions) Some(chain1block1) else None
    applyBlock(chain1block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val chain2block1 = validFullBlock(parentOpt = None, us, bh)
    applyBlock(chain2block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val wusChain2Block1 = wusGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)
    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    applyBlock(chain2block2) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockEncodedId shouldBe Some(chain2block2.header.encodedId)
    }

    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getRootHash shouldBe Algos.encode(chain2block2.header.stateRoot)
  }

  private val t17 = TestCase("apply invalid genesis header") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val header = validFullBlock(None, us, bh).header.copy(parentId = bytesToId(Array.fill(32)(9: Byte)))

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    nodeViewHolderRef ! LocallyGeneratedModifier[Header](header)
    expectMsgType[SyntacticallyFailedModification[Header]]
    getHistoryHeight shouldBe ErgoHistory.EmptyHistoryHeight
    getHeightOf(header.id) shouldBe None
  }

  private val t18 = TestCase("apply syntactically invalid genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))

    val validBlock = validFullBlock(parentOpt = None, us, bh)
    val invalidBlock = validBlock.copy(header = validBlock.header.copy(parentId = bytesToId(Array.fill(32)(9: Byte))))

    applyBlock(invalidBlock) shouldBe 'failure
    getBestFullBlockOpt shouldBe None
    getBestHeaderOpt shouldBe None
  }

  private val t19 = TestCase("apply semantically invalid genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val wusGenesis = WrappedUtxoState(us, bh, stateConstants)

    val invalidBlock = generateInvalidFullBlock(None, wusGenesis)

    if (verifyTransactions) {

      val initDigest = getCurrentState.rootHash

      applyBlock(invalidBlock) shouldBe 'success

      getBestFullBlockOpt shouldBe None
      getBestHeaderOpt shouldBe None
      getCurrentState.rootHash shouldEqual initDigest
    }
  }

  val cases: List[TestCase] = List(t1, t2, t3, t4, t5, t6, t7, t8, t9)

  NodeViewTestConfig.allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }

  val verifyingTxCases: List[TestCase] = List(t10, t11, t12, t13)

  NodeViewTestConfig.verifyTxConfigs.foreach { c =>
    verifyingTxCases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }

  val genesisIdTestCases = List(t14, t15, t16, t17, t18, t19)

  def genesisIdConfig(expectedGenesisIdOpt: Option[ModifierId])(protoSettings: ErgoSettings): ErgoSettings = {
    protoSettings.copy(chainSettings = protoSettings.chainSettings.copy(genesisId = expectedGenesisIdOpt))
  }

  genesisIdTestCases.foreach { t =>
    property(t.name) {
      t.run(NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = true))
    }
  }

}
