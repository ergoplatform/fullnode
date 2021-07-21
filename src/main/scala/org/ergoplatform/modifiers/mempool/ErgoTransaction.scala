package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxPropositionBytes}
import org.ergoplatform._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.utils.ArithUtils._
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{ErgoValidationSettings, Algos, ErgoAlgos}
import org.ergoplatform.utils.metrics._
import org.ergoplatform.utils.{metrics, BoxUtils}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.EphemerealNodeViewModifier
import org.ergoplatform.wallet.protocol.context.{InputContext, TransactionContext}
import scalan.util.BenchmarkUtil
import scorex.core.serialization.ScorexSerializer
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationResult.fromValidationState
import scorex.core.validation.{ValidationState, ModifierValidator, ValidationResult}
import scorex.db.{ByteArrayUtils, ByteArrayWrapper}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{bytesToId, ScorexLogging, ModifierId}
import sigmastate.Values.{SigmaPropConstant, SigmaBoolean, ErgoTree}
import sigmastate.eval.Extensions._
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.CostTable

import scala.collection.mutable
import scala.util.{Success, Failure, Try}

/**
  * ErgoTransaction is an atomic state transition operation. It destroys Boxes from the state
  * and creates new ones. If transaction is spending boxes protected by some non-trivial scripts,
  * its inputs should also contain proof of spending correctness - context extension (user-defined
  * key-value map) and data inputs (links to existing boxes in the state) that may be used during
  * script reduction to crypto, signatures that satisfies the remaining cryptographic protection
  * of the script.
  * Transactions are not encrypted, so it is possible to browse and view every transaction ever
  * collected into a block.
  *
  * @param inputs           - inputs, that will be spent by this transaction.
  * @param dataInputs       - inputs, that are not going to be spent by transaction, but will be
  *                         reachable from inputs scripts. `dataInputs` scripts will not be executed,
  *                         thus their scripts costs are not included in transaction cost and
  *                         they do not contain spending proofs.
  * @param outputCandidates - box candidates to be created by this transaction.
  *                         Differ from ordinary ones in that they do not include transaction id and index
  */
case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val dataInputs: IndexedSeq[DataInput],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate],
                           override val sizeOpt: Option[Int] = None)
  extends ErgoLikeTransaction(inputs, dataInputs, outputCandidates)
    with Transaction
    with EphemerealNodeViewModifier
    with ErgoNodeViewModifier
    with ScorexLogging {

  override val serializedId: Array[Byte] = Algos.hash(messageToSign)

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Id of transaction "witness" (taken from Bitcoin jargon, means commitment to signatures of a transaction).
    * Id is 248-bit long, to distinguish transaction ids from witness ids in Merkle tree of transactions,
    * where both kinds of ids are written into leafs of the tree.
    */
  lazy val witnessSerializedId: Array[Byte] =
    Algos.hash(ByteArrayUtils.mergeByteArrays(inputs.map(_.spendingProof.proof))).tail


  lazy val outAssetsTry: Try[(Map[ByteArrayWrapper, Long], Int)] = ErgoTransaction.extractAssets(outputCandidates)

  lazy val outputsSumTry: Try[Long] = Try(outputCandidates.map(_.value).reduce(Math.addExact(_, _)))

  /**
    * Stateless transaction validation with result returned as `ValidationResult`
    * to accumulate further validation results
    *
    * @note Consensus-critical!
    */
  def validateStateless(): ValidationState[Unit] = {
    ModifierValidator(ErgoValidationSettings.initial)
      .validate(txNoInputs, inputs.nonEmpty, s"$id")
      .validate(txNoOutputs, outputCandidates.nonEmpty, s"$id")
      .validate(txManyInputs, inputs.size <= Short.MaxValue, s"$id: ${inputs.size}")
      .validate(txManyDataInputs, dataInputs.size <= Short.MaxValue, s"$id: ${dataInputs.size}")
      .validate(txManyOutputs, outputCandidates.size <= Short.MaxValue, s"$id: ${outputCandidates.size}")
      .validate(txNegativeOutput, outputCandidates.forall(_.value >= 0), s"$id: ${outputCandidates.map(_.value)}")
      .validateNoFailure(txOutputSum, outputsSumTry)
      .validate(txInputsUnique, inputs.distinct.size == inputs.size, s"$id: ${inputs.distinct.size} == ${inputs.size}")
  }

  /**
    * Same as `validateStateless`, but result is returned as Try[Unit]
    **/
  def statelessValidity(): Try[Unit] = {
    validateStateless().result.toTry
  }

  val verifyScriptMetric = MetricDesc[InputMetricData]("verifyScript")

  /**
    * Checks whether transaction is valid against input boxes to spend, and
    * non-spendable data inputs.
    *
    * Note that this method make only checks which are possible when input boxes are available.
    *
    * To make full transaction validation, use (tx.statelessValidity && tx.statefulValidity(...))
    *
    * @note Consensus-critical!
    * @param boxesToSpend    - boxes the transaction spends (via inputs)
    * @param dataBoxes       - boxes the transaction only reads (via data inputs)
    * @param stateContext    - blockchain context at the moment of validation
    * @param accumulatedCost - computational cost before validation, validation starts with this value
    * @param verifier        - interpreter used to check spending correctness for transaction inputs
    * @return total computation cost (accumulatedCost + transaction verification cost), or error details
    */
  def validateStateful(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long)
                      (implicit verifier: ErgoInterpreter): ValidationState[Long] = {
    implicit val es = stateContext.ergoSettings
    verifier.IR.resetContext() // ensure there is no garbage in the IRContext
    lazy val inputSumTry = Try(boxesToSpend.map(_.value).reduce(Math.addExact(_, _)))

    val blockVersion = stateContext.blockVersion

    // Cost of transaction initialization: we should read and parse all inputs and data inputs,
    // and also iterate through all outputs to check rules
    val initialCost: Long = addExact(
      CostTable.interpreterInitCost,
      multiplyExact(boxesToSpend.size, stateContext.currentParameters.inputCost),
      multiplyExact(dataBoxes.size, stateContext.currentParameters.dataInputCost),
      multiplyExact(outputCandidates.size, stateContext.currentParameters.outputCost),
    )

    // Cost limit per block
    val maxCost = stateContext.currentParameters.maxBlockCost

    // We sum up previously accumulated cost and transaction initialization cost
    val startCost = addExact(initialCost, accumulatedCost)
    ModifierValidator(stateContext.validationSettings)
      // Check that the initial transaction cost is not too exceeding block limit
      .validate(bsBlockTransactionsCost, maxCost >= startCost, s"$id: initial cost")
      // Starting validation
      .payload(startCost)
      // Perform cheap checks first
      .validateNoFailure(txAssetsInOneBox, outAssetsTry)
      .validate(txPositiveAssets,
        outputCandidates.forall(_.additionalTokens.forall(_._2 > 0)),
        s"$id: ${outputCandidates.map(_.additionalTokens)}")
      // Check that outputs are not dust, and not created in future
      .validateSeq(outputs) { case (validationState, out) =>
        validationState
          .validate(txDust, out.value >= BoxUtils.minimalErgoAmount(out, stateContext.currentParameters),
            s"$id, output ${Algos.encode(out.id)}, ${out.value} >= ${BoxUtils.minimalErgoAmount(out, stateContext.currentParameters)}")
          .validate(txFuture, out.creationHeight <= stateContext.currentHeight,
            s" ${out.creationHeight} <= ${stateContext.currentHeight} is not true, output id: $id: output $out")
          .validate(txNegHeight, (blockVersion == 1) || out.creationHeight >= 0,
            s" ${out.creationHeight} >= 0 is not true, output id: $id: output $out")
          .validate(txBoxSize, out.bytes.length <= MaxBoxSize.value, s"$id: output $out")
          .validate(txBoxPropositionSize, out.propositionBytes.length <= MaxPropositionBytes.value, s"$id: output $out")
      }
      // Just to be sure, check that all the input boxes to spend (and to read) are presented.
      // Normally, this check should always pass, if the client is implemented properly
      // so it is not part of the protocol really.
      .validate(txBoxesToSpend, boxesToSpend.size == inputs.size, s"$id: ${boxesToSpend.size} == ${inputs.size}")
      .validate(txDataBoxes, dataBoxes.size == dataInputs.size, s"$id: ${dataBoxes.size} == ${dataInputs.size}")
      // Check that there are no overflow in input and output values
      .validate(txInputsSum, inputSumTry.isSuccess, s"$id")
      // Check that transaction is not creating money out of thin air.
      .validate(txErgPreservation, inputSumTry == outputsSumTry, s"$id: $inputSumTry == $outputsSumTry")
      .validateTry(outAssetsTry, e => ModifierValidator.fatal("Incorrect assets", e)) {
        case (validation, (outAssets, outAssetsNum)) =>
          val inAssetsTry = ErgoTransaction.extractAssets(boxesToSpend)
          inAssetsTry match {
            case Success((inAssets, inAssetsNum)) =>
              lazy val newAssetId = ByteArrayWrapper(inputs.head.boxId)
              val tokenAccessCost = stateContext.currentParameters.tokenAccessCost
              val currentTxCost = validation.result.payload.get
              // Cost of assets preservation rules checks.
              // We iterate through all assets to create a map (cost: `(outAssetsNum + inAssetsNum) * tokenAccessCost)`)
              // and after that we iterate through unique asset ids to check preservation rules (cost: `(inAssets.size + outAssets.size) * tokenAccessCost`)
              val totalAssetsAccessCost = (outAssetsNum + inAssetsNum) * tokenAccessCost +
                (inAssets.size + outAssets.size) * tokenAccessCost
              val newCost = addExact(currentTxCost, totalAssetsAccessCost)

              validation
                // Check that transaction is not too costly considering all the assets
                .validate(bsBlockTransactionsCost, maxCost >= newCost, s"$id: assets cost")
                .validateSeq(outAssets) {
                  case (validationState, (outAssetId, outAmount)) =>
                    val inAmount: Long = inAssets.getOrElse(outAssetId, -1L)

                    // Check that for each asset output amount is no more than input amount,
                    // with a possible exception for a new asset created by the transaction
                    validationState.validate(txAssetsPreservation,
                      inAmount >= outAmount || (outAssetId == newAssetId && outAmount > 0),
                      s"$id: Amount in = $inAmount, out = $outAmount. Allowed new asset = $newAssetId, out = $outAssetId")
                }
                .payload(newCost)
            case Failure(e) =>
              // should never be here as far as we've already checked this when we've created the box
              ModifierValidator.fatal(e.getMessage)
          }
      }
      // Check inputs, the most expensive check usually, so done last.
      .validateSeq(boxesToSpend.zipWithIndex) { case (validation, (box, idx)) =>
        if (es.nodeSettings.collectMetrics) {
          val (res, time) = measureTimeNano {
            validateInput(validation, boxesToSpend, dataBoxes, stateContext, box, idx)
          }

          val currentTxCost = validation.result.payload.get
          collectMetricsTo(
            verifyScriptMetric,
            data = InputMetricData(
              blockId = stateContext.lastHeaderIdOpt,
              txId = id,
              index = idx),
            cost = res.map { c =>
              val scriptCost = c - currentTxCost // cost of script validation for this input
              scriptCost
            }.toTry.getOrElse(-1L),
            time
          )
          res
        } else {
          validateInput(validation, boxesToSpend, dataBoxes, stateContext, box, idx)
        }
      }
  }

  private def validateInput(validation: ValidationState[Long],
                    boxesToSpend: IndexedSeq[ErgoBox],
                    dataBoxes: IndexedSeq[ErgoBox],
                    stateContext: ErgoStateContext,
                    box: ErgoBox, idx: Int)
                   (implicit verifier: ErgoInterpreter): ValidationResult[Long] = {
    val input = inputs(idx)
    val proof = input.spendingProof
    val transactionContext = TransactionContext(boxesToSpend, dataBoxes, this)
    val inputContext = InputContext(idx.toShort, proof.extension)

    // Cost limit per block
    val maxCost = stateContext.currentParameters.maxBlockCost
    val currentTxCost = validation.result.payload.get

    val ctx = new ErgoContext(
      stateContext, transactionContext, inputContext,
      costLimit = maxCost - currentTxCost, // remaining cost so far
      initCost = 0)

    val costTry = verifier.verify(box.ergoTree, ctx, proof, messageToSign)
    costTry.recover { case t =>
      log.debug(s"Tx verification failed: ${t.getMessage}")
    }

    val (isCostValid, scriptCost) = costTry.getOrElse((false, maxCost + 1))

    val currCost = addExact(currentTxCost, scriptCost)

    validation
      // Just in case, should always be true if client implementation is correct.
      .validateEquals(txBoxToSpend, box.id, input.boxId)
      // Check whether input box script interpreter raised exception
      .validate(txScriptValidation, costTry.isSuccess && isCostValid, s"$id: #$idx => $costTry")
      // Check that cost of the transaction after checking the input becomes too big
      .validate(bsBlockTransactionsCost, currCost <= maxCost, s"$id: cost exceeds limit after input #$idx")
      .map(c => addExact(c, scriptCost))
  }

  /**
    * Same as `validateStateful`, but result is returned as Try[Long]
    **/
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox],
                       dataBoxes: IndexedSeq[ErgoBox],
                       stateContext: ErgoStateContext,
                       accumulatedCost: Long = 0L)
                      (implicit verifier: ErgoInterpreter): Try[Long] = {
    validateStateful(boxesToSpend, dataBoxes, stateContext, accumulatedCost).result.toTry
  }

  override type M = ErgoTransaction

  override def serializer: ScorexSerializer[ErgoTransaction] = ErgoTransactionSerializer

  override def toString: String = {
    import ErgoTransaction._
    val displayMaxObjects = 3
    val inputsStr = if (inputs.size > displayMaxObjects) {
      inputs.take(displayMaxObjects).asJson.noSpaces + s" ... (${inputs.size})"
    } else {
      inputs.asJson.noSpaces
    }
    val outputsStr = if (outputs.size > displayMaxObjects) {
      outputs.take(displayMaxObjects).asJson.noSpaces + s" ... (${outputs.size})"
    } else {
      outputs.asJson.noSpaces
    }
    s"ErgoTransaction(id: $encodedId, inputs: $inputsStr, outputs: $outputsStr, size: $size)"
  }

}

object ErgoTransaction extends ApiCodecs with ScorexLogging with ScorexEncoding {

  val MaxAssetsPerBox = 255

  /**
    * Extracts a mapping (assets -> total amount) from a set of boxes passed as a parameter.
    * That is, the method is checking amounts of assets in the boxes(i.e. that a box contains positive
    * amount for an asset) and then summarize and group their corresponding amounts.
    *
    * @param boxes - boxes to check and extract assets from
    * @return a mapping from asset id to to balance and total assets number
    */
  def extractAssets(boxes: IndexedSeq[ErgoBoxCandidate]): Try[(Map[ByteArrayWrapper, Long], Int)] = Try {
    val map: mutable.Map[ByteArrayWrapper, Long] = mutable.Map[ByteArrayWrapper, Long]()
    val assetsNum = boxes.foldLeft(0) { case (acc, box) =>
      require(box.additionalTokens.length <= ErgoTransaction.MaxAssetsPerBox, "too many assets in one box")
      box.additionalTokens.foreach { case (assetId, amount) =>
        val aiWrapped = ByteArrayWrapper(assetId)
        val total = map.getOrElse(aiWrapped, 0L)
        map.put(aiWrapped, Math.addExact(total, amount))
      }
      acc + box.additionalTokens.size
    }
    map.toMap -> assetsNum
  }

  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoTransaction =
    ErgoTransaction(inputs, IndexedSeq(), outputCandidates, None)

  def apply(tx: ErgoLikeTransaction): ErgoTransaction =
    ErgoTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)

}

object ErgoTransactionSerializer extends ScorexSerializer[ErgoTransaction] {

  override def serialize(tx: ErgoTransaction, w: Writer): Unit = {
    val elt = new ErgoLikeTransaction(tx.inputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(elt, new SigmaByteWriter(w, None))
  }

  override def parse(r: Reader): ErgoTransaction = {
    val reader = new SigmaByteReader(r,
      new ConstantStore(),
      resolvePlaceholdersToConstants = false)
    val elt = ErgoLikeTransactionSerializer.parse(reader)
    ErgoTransaction(elt.inputs, elt.dataInputs, elt.outputCandidates)
  }
}
