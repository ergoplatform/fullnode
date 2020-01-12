package org.ergoplatform.nodeView.state

import java.util.concurrent.Executors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, R4}
import org.ergoplatform._
import org.ergoplatform.mining._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import scorex.core._
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigmastate.Values.ByteArrayConstant
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.ProverResult

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Random, Try}


class UtxoStateSpecification extends ErgoPropertyTest with ErgoTransactionGenerators {

  property("Founders box workflow") {
    var (us, bh) = createUtxoState()
    var foundersBox = genesisBoxes.last
    var lastBlock = validFullBlock(parentOpt = None, us, bh)
    us = us.applyModifier(lastBlock).get

    // spent founders box, leaving the same proposition
    (0 until 10) foreach { _ =>
      val height = us.stateContext.currentHeight
      val inputs = IndexedSeq(Input(foundersBox.id, emptyProverResult))
      val remaining = emission.remainingFoundationRewardAtHeight(height)
      val newFoundersBox = ErgoBox(remaining, foundersBox.ergoTree, height, Seq(), Map(R4 -> foundersBox.additionalRegisters(R4)))
      val rewardBox = ErgoBox(foundersBox.value - remaining, defaultProver.secrets.last.publicImage, height)
      val newBoxes = IndexedSeq(newFoundersBox, rewardBox)
      val unsignedTx = new UnsignedErgoTransaction(inputs, IndexedSeq(), newBoxes)
      val tx: ErgoTransaction = ErgoTransaction(defaultProver.sign(unsignedTx, IndexedSeq(foundersBox), emptyDataBoxes, us.stateContext).get)
      us.validateWithCost(tx, None, Constants.DefaultComplexityLimit).get should be <= 100000L
      val block1 = validFullBlock(Some(lastBlock), us, Seq(ErgoTransaction(tx)))
      us = us.applyModifier(block1).get
      foundersBox = tx.outputs.head
      lastBlock = block1
    }
  }

  property("Founders should be able to spend genesis founders box") {
    var (us, bh) = createUtxoState()
    val foundersBox = genesisBoxes.last
    var height: Int = ErgoHistory.GenesisHeight

    val settingsPks = settings.chainSettings.foundersPubkeys
      .map(str => groupElemFromBytes(Base16.decode(str).get))
      .map(pk => ProveDlog(pk))
    settingsPks.count(defaultProver.pubKeys.contains) shouldBe 2

    forAll(defaultHeaderGen) { header =>
      val rewardPk = new DLogProverInput(BigInt(header.height).bigInteger).publicImage

      val t = validTransactionsFromBoxHolder(bh, new Random(height))
      val txs = t._1
      bh = t._2
      val (adProofBytes, adDigest) = us.proofsForTransactions(txs).get
      val realHeader = header.copy(stateRoot = adDigest,
        ADProofsRoot = ADProofs.proofDigest(adProofBytes),
        height = height,
        parentId = us.stateContext.lastHeaderOpt.map(_.id).getOrElse(Header.GenesisParentId))
      val adProofs = ADProofs(realHeader.id, adProofBytes)
      val fb = ErgoFullBlock(realHeader, BlockTransactions(realHeader.id, txs), genExtension(realHeader, us.stateContext), Some(adProofs))
      us = us.applyModifier(fb).get
      val remaining = emission.remainingFoundationRewardAtHeight(height)

      // check validity of transaction, spending founders box
      val inputs = IndexedSeq(Input(foundersBox.id, emptyProverResult))
      val newBoxes = IndexedSeq(
        ErgoBox(remaining, foundersBox.ergoTree, height, Seq(), foundersBox.additionalRegisters),
        ErgoBox(foundersBox.value - remaining, rewardPk, height, Seq())
      )
      val unsignedTx = new UnsignedErgoTransaction(inputs, IndexedSeq(), newBoxes)
      val tx = defaultProver.sign(unsignedTx, IndexedSeq(foundersBox), emptyDataBoxes, us.stateContext).get
      us.validate(ErgoTransaction(tx)) shouldBe 'success
      height = height + 1
    }
  }

  property("Correct genesis state") {
    val (us, bh) = createUtxoState()
    val boxes = bh.boxes.values.toList
    boxes.size shouldBe 3

    // check tests consistency
    genesisBoxes.length shouldBe bh.boxes.size
    genesisBoxes.foreach { b =>
      us.boxById(b.id).isDefined shouldBe true
      bh.boxes.get(ByteArrayWrapper(b.id)).isDefined shouldBe true
    }

    // check total supply
    boxes.map(_.value).sum shouldBe coinsTotal

    // boxes should contain all no-premine proofs in registers
    val additionalRegisters = boxes.flatMap(_.additionalRegisters.values)
    initSettings.chainSettings.noPremineProof.foreach { pStr =>
      val pBytes = ByteArrayConstant(pStr.getBytes("UTF-8"))
      additionalRegisters should contain(pBytes)
    }

  }

  property("extractEmissionBox() should extract correct box") {
    var (us, bh) = createUtxoState()
    us.emissionBoxOpt should not be None
    var lastBlockOpt: Option[ErgoFullBlock] = None
    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(lastBlockOpt, us, bh, new Random(seed))
      val block = blBh._1
      us.extractEmissionBox(block) should not be None
      lastBlockOpt = Some(block)
      bh = blBh._2
      us = us.applyModifier(block).get
    }
  }

  property("fromBoxHolder") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.take(1000)._1.foreach { box =>
        us.boxById(box.id) shouldBe Some(box)
      }
    }
  }

  property("proofsForTransactions") {
    var (us: UtxoState, bh) = createUtxoState()
    var height: Int = ErgoHistory.GenesisHeight
    forAll(defaultHeaderGen) { header =>
      val t = validTransactionsFromBoxHolder(bh, new Random(height))
      val txs = t._1
      bh = t._2
      val (adProofBytes, adDigest) = us.proofsForTransactions(txs).get
      val realHeader = header.copy(stateRoot = adDigest,
        ADProofsRoot = ADProofs.proofDigest(adProofBytes),
        height = height,
        parentId = us.stateContext.lastHeaderOpt.map(_.id).getOrElse(Header.GenesisParentId))
      val adProofs = ADProofs(realHeader.id, adProofBytes)
      val fb = ErgoFullBlock(realHeader, BlockTransactions(realHeader.id, txs), genExtension(realHeader, us.stateContext), Some(adProofs))
      us = us.applyModifier(fb).get
      height = height + 1
    }
  }

  property("concurrent applyModifier() and proofsForTransactions()") {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))


      var bh = BoxHolder(Seq(genesisEmissionBox))
      var state = createUtxoState(bh)

      var height: Int = ErgoHistory.GenesisHeight
      // generate chain of correct full blocks
      val chain = (0 until 10) map { _ =>
        val header = defaultHeaderGen.sample.value
        val t = validTransactionsFromBoxHolder(bh, new Random(height))
        val txs = t._1
        bh = t._2
        val (adProofBytes, adDigest) = state.proofsForTransactions(txs).get
        val realHeader = header.copy(stateRoot = adDigest,
          ADProofsRoot = ADProofs.proofDigest(adProofBytes),
          height = height,
          parentId = state.stateContext.lastHeaderOpt.map(_.id).getOrElse(Header.GenesisParentId))
        val adProofs = ADProofs(realHeader.id, adProofBytes)
        height = height + 1
        val fb = ErgoFullBlock(realHeader, BlockTransactions(realHeader.id, txs), genExtension(realHeader, state.stateContext), Some(adProofs))
        state = state.applyModifier(fb).get
        fb
      }
      // create new genesis state
      var state2 = createUtxoState(BoxHolder(Seq(genesisEmissionBox)))
      val state2Reader = state2.getReader.asInstanceOf[UtxoState]

      // parallel thread that generates proofs
      Future {
        (0 until 1000) foreach { _ =>
          Try {
            val boxes = state2Reader.randomBox().toSeq
            val txs = validTransactionsFromBoxes(400, boxes, new Random)._1
            state2Reader.proofsForTransactions(txs).get
            state2Reader.persistentProver.checkTree(true)
            state2Reader.persistentProver.checkTree(false)
          }
        }
      }

      // apply chain of headers full block to state
      // "Calculated proofHash is not equal to the declared one" UtxoState: 121
      chain.foreach { fb =>
        state2 = state2.applyModifier(fb).get
      }

  }

  property("proofsForTransactions() to be deterministic") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      val txs = validTransactionsFromBoxHolder(bh)._1

      val (proof1, digest1) = us.proofsForTransactions(txs).get
      val (proof2, digest2) = us.proofsForTransactions(txs).get

      ADProofs.proofDigest(proof1) shouldBe ADProofs.proofDigest(proof2)
      digest1 shouldBe digest2
    }
  }

  property("applyTransactions() - simple case for transaction with dataInputs") {
    forAll(boxesHolderGen) { bh =>
      val txsIn = validTransactionsFromBoxHolder(bh)._1
      val headTx = txsIn.head
      val us = createUtxoState(bh)
      val existingBoxes: IndexedSeq[BoxId] = bh.boxes.takeRight(3).map(_._2.id).toIndexedSeq

      // trying to apply transactions with missing data inputs
      val missedId: BoxId = ADKey @@ scorex.util.Random.randomBytes()
      us.boxById(missedId) shouldBe None
      val missingDataInputs = (missedId +: existingBoxes).map(DataInput).toIndexedSeq
      val txWithMissedDataInputs = ErgoTransaction(headTx.inputs, missingDataInputs, headTx.outputCandidates)
      val incorrectTransactions = IndexedSeq(txWithMissedDataInputs)
      // proof for transaction works correctly, providing proof-of-non-existence for missed input
      val digest2 = us.proofsForTransactions(incorrectTransactions).get._2
      us.applyTransactions(incorrectTransactions, digest2, emptyStateContext) shouldBe 'failure

      // trying to apply transactions with correct data inputs
      val existingDataInputs = existingBoxes.map(DataInput).toIndexedSeq
      existingDataInputs.foreach(b => us.boxById(b.boxId) should not be None)
      val txWithDataInputs = ErgoTransaction(headTx.inputs, existingDataInputs, headTx.outputCandidates)
      val correctTransactions = IndexedSeq(txWithDataInputs)
      val digest = us.proofsForTransactions(correctTransactions).get._2
      us.applyTransactions(correctTransactions, digest, emptyStateContext).get
    }
  }

  property("applyTransactions() - dataInputs intersect with inputs") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)

      // generate 2 independent transactions, that only spend state boxes
      val headTx = validTransactionsFromBoxes(1, bh.boxes.take(10).values.toSeq, new Random())._1.head
      val nextTx = validTransactionsFromBoxes(1, bh.boxes.takeRight(10).values.toSeq, new Random())._1.head
      headTx.inputs.intersect(nextTx.inputs) shouldBe empty

      // trying to apply transactions with data inputs same as inputs of the next tx
      val dataInputs = nextTx.inputs.filter(i => us.boxById(i.boxId).isDefined).map(i => DataInput(i.boxId))
      val txWithDataInputs = ErgoTransaction(headTx.inputs, dataInputs, headTx.outputCandidates)

      val txs1 = IndexedSeq(headTx, nextTx)
      val txs2 = IndexedSeq(txWithDataInputs, nextTx)
      val sc1 = ErgoState.stateChanges(txs1)
      val sc2 = ErgoState.stateChanges(IndexedSeq(txWithDataInputs, nextTx))
      // check that the only difference between txs1 and txs2 are dataInputs and Lookup tree operations
      txs1.flatMap(_.inputs) shouldBe txs2.flatMap(_.inputs)
      txs1.flatMap(_.outputCandidates) shouldBe txs2.flatMap(_.outputCandidates)
      sc1.toAppend.size shouldBe sc2.toAppend.size
      sc1.toRemove shouldBe sc2.toRemove

      us.proofsForTransactions(txs1) shouldBe 'success
      us.proofsForTransactions(txs2) shouldBe 'success

      val inputs = headTx.outputs.map(b => Input(b.id, ProverResult.empty))
      val txWithDataInputs2 = ErgoTransaction(inputs, dataInputs, headTx.outputCandidates)

      val version = us.version

      val txs3 = IndexedSeq(headTx, nextTx, txWithDataInputs2)
      val (_, digest3) = us.proofsForTransactions(txs3).get
      us.applyTransactions(txs3, digest3, emptyStateContext) shouldBe 'success
      us.rollbackTo(version)

      val txs4 = IndexedSeq(headTx, txWithDataInputs2, nextTx)
      val (_, digest4) = us.proofsForTransactions(txs4).get
      us.applyTransactions(txs4, digest4, emptyStateContext) shouldBe 'success
      us.rollbackTo(version)

      val txs5 = IndexedSeq(txWithDataInputs2, headTx, nextTx)
      us.proofsForTransactions(txs5) shouldBe 'failure
      us.applyTransactions(txs5, digest4, emptyStateContext) shouldBe 'failure
      us.rollbackTo(version)

      // trying to apply transactions with data inputs same as outputs of the previous tx
      val dataInputsNext = headTx.outputs.take(1).map(i => DataInput(i.id))
      dataInputsNext should not be empty
      val nextTxWithDataInputs = ErgoTransaction(nextTx.inputs, dataInputsNext, nextTx.outputCandidates)
      val txsNext = IndexedSeq(headTx, nextTxWithDataInputs)
      // proof of non-existence
      val d2 = us.proofsForTransactions(txsNext).get._2
      us.applyTransactions(txsNext, d2, emptyStateContext) shouldBe 'success
    }
  }

  property("applyTransactions() - simple case") {
    forAll(boxesHolderGen) { bh =>
      val txs = validTransactionsFromBoxHolder(bh)._1

      val created = txs.flatMap(_.outputs.map(_.id)).map(ByteArrayWrapper.apply)
      val boxIds = txs.flatMap(_.inputs.map(_.boxId)).map(ByteArrayWrapper.apply)
      boxIds.distinct.size shouldBe boxIds.size
      val toRemove = boxIds.filterNot(id => created.contains(id))
      toRemove.foreach(id => bh.get(id) should not be None)

      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)
      val digest = us.proofsForTransactions(txs).get._2
      val wBlock = invalidErgoFullBlockGen.sample.get
      val block = wBlock.copy(header = wBlock.header.copy(height = 1))
      val newSC = us.stateContext.appendFullBlock(block, votingSettings).get
      us.applyTransactions(txs, digest, newSC).get
    }
  }

  property("applyTransactions() - a transaction is spending an output created by a previous transaction") {
    forAll(boxesHolderGen) { bh =>
      val txsFromHolder = validTransactionsFromBoxHolder(bh)._1

      val boxToSpend = txsFromHolder.last.outputs.head

      val spendingTxInput = Input(boxToSpend.id, emptyProverResult)
      val spendingTx = ErgoTransaction(
        IndexedSeq(spendingTxInput),
        IndexedSeq(),
        IndexedSeq(new ErgoBoxCandidate(boxToSpend.value, Constants.TrueLeaf, creationHeight = startHeight))
      )

      val txs = txsFromHolder :+ spendingTx

      val us = createUtxoState(bh)
      val digest = us.proofsForTransactions(txs).get._2

      val header = invalidHeaderGen.sample.get.copy(stateRoot = digest, height = 1)
      val fb = new ErgoFullBlock(header, new BlockTransactions(header.id, txs), genExtension(header, us.stateContext), None)
      val newSC = us.stateContext.appendFullBlock(fb, votingSettings).get
      us.applyTransactions(txs, digest, newSC).get
    }
  }

  property("proofsForTransactions() fails if a transaction is spending an output created by a follow-up transaction") {
    forAll(boxesHolderGen) { bh =>
      val txsFromHolder = validTransactionsFromBoxHolder(bh)._1

      val boxToSpend = txsFromHolder.last.outputs.head

      val spendingTxInput = Input(boxToSpend.id, emptyProverResult)
      val spendingTx = ErgoTransaction(
        IndexedSeq(spendingTxInput),
        IndexedSeq(),
        IndexedSeq(new ErgoBoxCandidate(boxToSpend.value, Constants.TrueLeaf, creationHeight = startHeight)))

      val txs = spendingTx +: txsFromHolder

      val us = createUtxoState(bh)
      us.proofsForTransactions(txs).isSuccess shouldBe false
    }
  }

  property("applyModifier() - valid full block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)
      us.applyModifier(block).get
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createUtxoState()._1
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid full block after invalid one") {
    val (us, bh) = createUtxoState()
    val validBlock = validFullBlock(parentOpt = None, us, bh)

    //Different state
    val (us2, bh2) = {
      lazy val initialBoxes: Seq[ErgoBox] = (1 to 1).map(_ => ergoBoxGenNoProp.sample.get)

      val bh = BoxHolder(initialBoxes)

      createUtxoState(bh) -> bh
    }
    val invalidBlock = validFullBlock(parentOpt = None, us2, bh2)

    us.applyModifier(invalidBlock).isSuccess shouldBe false
    us.applyModifier(validBlock).isSuccess shouldBe true
  }


  property("2 forks switching") {
    val (us, bh) = createUtxoState()
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    val chain1block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    val wusChain1Block1 = wusAfterGenesis.applyModifier(chain1block1).get
    val chain1block2 = validFullBlock(Some(chain1block1), wusChain1Block1)

    val (us2, bh2) = createUtxoState()
    val wus2AfterGenesis = WrappedUtxoState(us2, bh2, stateConstants).applyModifier(genesis).get
    val chain2block1 = validFullBlock(Some(genesis), wus2AfterGenesis)
    val wusChain2Block1 = wus2AfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)

    var (state, _) = createUtxoState()
    state = state.applyModifier(genesis).get

    state = state.applyModifier(chain1block1).get

    state = state.rollbackTo(idToVersion(genesis.id)).get
    state = state.applyModifier(chain2block1).get
    state = state.applyModifier(chain2block2).get

    state = state.rollbackTo(idToVersion(genesis.id)).get
    state = state.applyModifier(chain1block1).get
    state = state.applyModifier(chain1block2).get

  }

  property("rollback n blocks and apply again") {
    forAll(boxesHolderGen, smallPositiveInt) { (bh, depth) =>
      whenever(depth > 0 && depth <= 5) {
        val us = createUtxoState(bh)
        bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)
        val genesis = validFullBlock(parentOpt = None, us, bh)
        val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
        wusAfterGenesis.rootHash shouldEqual genesis.header.stateRoot

        val (finalState: WrappedUtxoState, chain: Seq[ErgoFullBlock]) = (0 until depth)
          .foldLeft((wusAfterGenesis, Seq(genesis))) { (sb, _) =>
            val state = sb._1
            val block = validFullBlock(parentOpt = Some(sb._2.last), state)
            (state.applyModifier(block).get, sb._2 ++ Seq(block))
          }
        val finalRoot = finalState.rootHash
        finalRoot shouldEqual chain.last.header.stateRoot

        val rollbackedState = finalState.rollbackTo(idToVersion(genesis.id)).get
        rollbackedState.rootHash shouldEqual genesis.header.stateRoot

        val finalState2: WrappedUtxoState = chain.tail.foldLeft(rollbackedState) { (state, block) =>
          state.applyModifier(block).get
        }

        finalState2.rootHash shouldEqual finalRoot
      }
    }
  }


  private def genExtension(header: Header, sc: ErgoStateContext): Extension = {
    interlinksToExtension(updateInterlinks(sc.lastHeaderOpt, sc.lastExtensionOpt)).toExtension(header.id)
  }

}
