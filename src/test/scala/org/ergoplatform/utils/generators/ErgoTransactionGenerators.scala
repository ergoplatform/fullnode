package org.ergoplatform.utils.generators

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext, VotingData}
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionSigningRequest}
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, WalletTransaction}
import org.ergoplatform.settings.Parameters._
import org.ergoplatform.settings.{Constants, LaunchParameters, Parameters}
import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.UnsignedInput
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform.wallet.utils.Generators
import org.ergoplatform.{DataInput, ErgoAddress, ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, Input, P2PKAddress}
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.ByteArrayWrapper
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

trait ErgoTransactionGenerators extends ErgoGenerators with Generators {

  protected implicit val ergoAddressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  val creationHeightGen: Gen[Int] = Gen.choose(0, Int.MaxValue / 2)

  lazy val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    h <- creationHeightGen
    prop <- trueLeafGen
    ar <- additionalRegistersGen
    tokens <- additionalTokensGen
    value <- validValueGen(prop, tokens, ar)
  } yield new ErgoBoxCandidate(value, prop, h, tokens.toColl, ar)

  def ergoAddressGen: Gen[ErgoAddress] = proveDlogGen.map(P2PKAddress.apply)

  def ergoBoxCandidateGen(prop: ProveDlog): Gen[ErgoBoxCandidate] = for {
    h <- creationHeightGen
    ar <- additionalRegistersGen
    tokens <- additionalTokensGen
    value <- validValueGen(prop, tokens, ar)
  } yield new ErgoBoxCandidate(value, prop, h, tokens.toColl, ar)

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = ergoBoxGen(propGen = trueLeafGen)

  def ergoBoxGenForTokens(tokens: Seq[(TokenId, Long)],
                          propositionGen: Gen[ErgoTree]): Gen[ErgoBox] = {
    ergoBoxGen(propGen = propositionGen, tokensGen = Gen.oneOf(tokens, tokens), heightGen = ErgoHistory.EmptyHistoryHeight)
  }

  def unspendableErgoBoxGen(minValue: Long = LaunchParameters.minValuePerByte * 200,
                            maxValue: Long = coinsTotal): Gen[ErgoBox] = {
    ergoBoxGen(propGen = falseLeafGen, valueGenOpt = Some(Gen.choose(minValue, maxValue)))
  }

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    spendingProof <- noProofGen
  } yield Input(boxId, spendingProof)

  lazy val dataInputGen: Gen[DataInput] = for {
    boxId <- boxIdGen
  } yield DataInput(boxId)

  lazy val reallySmallInt: Gen[Int] = Gen.choose(0, 3)

  lazy val invalidErgoTransactionGen: Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    dataInputs: IndexedSeq[DataInput] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, dataInputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, ergoBoxCandidateGen).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, dataInputs, to)

  def invalidErgoTransactionGen(prop: ProveDlog): Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    dataInputs: IndexedSeq[DataInput] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, dataInputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, ergoBoxCandidateGen(prop)).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, dataInputs, to)

  lazy val walletTransactionGen: Gen[WalletTransaction] = for {
    tx <- invalidErgoTransactionGen
    inclusionHeight <- Gen.posNum[Int]
    scanId <- ScanId @@ Gen.posNum[Short]
  } yield WalletTransaction(tx, inclusionHeight, Seq(scanId))

  lazy val augWalletTransactionGen: Gen[AugWalletTransaction] = for {
    tx <- walletTransactionGen
    numConfirmation <- Gen.posNum[Int]
  } yield AugWalletTransaction(tx, numConfirmation)

  /**
    * Generates a transaction that is valid if correct boxes were provided.
    * Generated transaction may still be invalid, if:
    * - default prover does not know how to sign at least one input
    * - number of assets exceeds Transaction.MaxTokens
    */
  def validUnsignedTransactionFromBoxes(boxesToSpend: IndexedSeq[ErgoBox],
                                        rnd: Random = new Random,
                                        issueNew: Boolean = true,
                                        outputsProposition: ErgoTree = Constants.TrueLeaf,
                                        stateCtxOpt: Option[ErgoStateContext] = None,
                                        dataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()): UnsignedErgoTransaction = {
    require(boxesToSpend.nonEmpty, "At least one box is needed to generate a transaction")

    val inputSum = boxesToSpend.map(_.value).reduce(Math.addExact(_, _))
    val assetsMap: mutable.Map[ByteArrayWrapper, Long] =
      mutable.Map(boxesToSpend.flatMap(_.additionalTokens.toArray).map { case (bs, amt) =>
        ByteArrayWrapper(bs) -> amt
      }: _*)

    //randomly creating a new asset
    if (rnd.nextBoolean() && issueNew) {
      assetsMap.put(ByteArrayWrapper(boxesToSpend.head.id), rnd.nextInt(Int.MaxValue))
    }

    val minValue = BoxUtils.sufficientAmount(LaunchParameters)

    require(inputSum >= minValue)
    val inputsCount = boxesToSpend.size
    val maxOutputs = Math.min(Short.MaxValue, inputSum / minValue).toInt
    val outputsCount = Math.min(maxOutputs, Math.max(inputsCount + 1, rnd.nextInt(inputsCount * 2)))
    require(outputsCount > 0, s"outputs count is not positive: $outputsCount")

    require(minValue * outputsCount <= inputSum)
    val outputPreamounts = (1 to outputsCount).map(_ => minValue.toLong).toBuffer

    var remainder = inputSum - minValue * outputsCount
    do {
      val idx = Random.nextInt(outputsCount)
      if (remainder < inputSum / inputsCount) {
        outputPreamounts.update(idx, outputPreamounts(idx) + remainder)
        remainder = 0
      } else {
        val value = Math.abs(rnd.nextLong()) % (remainder / outputsCount)
        outputPreamounts.update(idx, outputPreamounts(idx) + value)
        remainder = remainder - value
      }
    } while (remainder > 0)

    val outputAmounts = outputPreamounts.toIndexedSeq

    val tokenAmounts: mutable.IndexedSeq[mutable.Map[ByteArrayWrapper, Long]] =
      mutable.IndexedSeq.fill(outputsCount)(mutable.Map[ByteArrayWrapper, Long]())

    var availableTokenSlots = outputsCount * ErgoBox.MaxTokens

    if (assetsMap.nonEmpty) {
      do {
        val in = assetsMap.head
        val outIdx = Stream.from(1, 1).map(_ => rnd.nextInt(tokenAmounts.size))
          .find(idx => tokenAmounts(idx).size < ErgoBox.MaxTokens).get
        val out = tokenAmounts(outIdx)
        val contains = out.contains(in._1)

        val amt = if (in._2 == 1 || (availableTokenSlots < assetsMap.size * 2 && !contains) || rnd.nextBoolean()) {
          in._2
        } else {
          Math.max(1, Math.min((rnd.nextDouble() * in._2).toLong, in._2))
        }

        if (amt == in._2) assetsMap.remove(in._1) else assetsMap.update(in._1, in._2 - amt)
        if (contains) {
          val outAmt = out(in._1)
          out.update(in._1, outAmt + amt)
        } else {
          availableTokenSlots = availableTokenSlots - 1
          out.update(in._1, amt)
        }
        tokenAmounts(outIdx) = out
      } while (assetsMap.nonEmpty && availableTokenSlots > 0)
    }

    val newBoxes = outputAmounts.zip(tokenAmounts.toIndexedSeq).map { case (amt, tokens) =>
      val normalizedTokens = tokens.toSeq.map(t => (Digest32 @@ t._1.data) -> t._2)
      testBox(amt, outputsProposition, 0, normalizedTokens)
    }
    val inputs = boxesToSpend.map(b => Input(b.id, emptyProverResult))
    val dataInputs = dataBoxes.map(b => DataInput(b.id))
    val unsignedTx = UnsignedErgoTransaction(inputs, dataInputs, newBoxes)
    require(unsignedTx.dataInputs.length == dataBoxes.length, s"${unsignedTx.dataInputs.length} == ${dataBoxes.length}")
    unsignedTx
  }

  def validTransactionFromBoxes(boxesToSpend: IndexedSeq[ErgoBox],
                                rnd: Random = new Random,
                                issueNew: Boolean = true,
                                outputsProposition: ErgoTree = Constants.TrueLeaf,
                                stateCtxOpt: Option[ErgoStateContext] = None,
                                dataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()): ErgoTransaction = {
    val unsignedTx = validUnsignedTransactionFromBoxes(boxesToSpend, rnd, issueNew, outputsProposition, stateCtxOpt, dataBoxes)
    defaultProver.sign(unsignedTx, boxesToSpend, dataBoxes, stateCtxOpt.getOrElse(emptyStateContext))
      .map(ErgoTransaction.apply)
      .getOrElse {
        log.debug(s"Going to generate a transaction with incorrect spending proofs: $unsignedTx")
        ErgoTransaction(boxesToSpend.map(b => Input(b.id, emptyProverResult)), unsignedTx.dataInputs, unsignedTx.outputs)
      }
  }

  def disperseTokens(inputsCount: Int, tokensCount: Byte): Gen[IndexedSeq[Seq[(TokenId, Long)]]] = {
    val tokensDistribution = mutable.IndexedSeq.fill(inputsCount)(Seq[(TokenId, Long)]())
    (1 to tokensCount).foreach { i =>
      val (id, amt) = Blake2b256(s"$i" + Random.nextString(5)) -> (Random.nextInt(Int.MaxValue).toLong + 100)
      val idx = i % tokensDistribution.size
      val s = tokensDistribution(idx)
      tokensDistribution(idx) = s :+ (id, amt)
    }
    tokensDistribution.ensuring(_.forall(_.forall(_._2 > 0)))
  }

  private def boxesGenTemplate(minAssets: Int,
                               maxAssets: Int = -1,
                               minInputs: Int = 1,
                               maxInputs: Int = 100,
                               propositionGen: Gen[ErgoTree] = trueLeafGen
                              ): Gen[(IndexedSeq[ErgoBox], ErgoTree)] = for {
    inputsCount <- Gen.choose(minInputs, maxInputs)
    tokensCount <- Gen.choose(
      minAssets,
      Math.max(maxAssets, inputsCount))
    tokensDistribution <- disperseTokens(inputsCount, tokensCount.toByte)
    from <- Gen.sequence(tokensDistribution.map(tokens => ergoBoxGenForTokens(tokens, propositionGen)))
    prop <- propositionGen
  } yield from.asScala.toIndexedSeq -> prop

  def validErgoTransactionGenTemplate(minAssets: Int,
                                      maxAssets: Int = -1,
                                      minInputs: Int = 1,
                                      maxInputs: Int = 100,
                                      propositionGen: Gen[ErgoTree] = trueLeafGen
                                     ): Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = {
    boxesGenTemplate(minAssets, maxAssets, maxInputs, maxInputs, propositionGen).map { case (boxes, prop) =>
      val tx = validTransactionFromBoxes(boxes, outputsProposition = prop)
      boxes -> tx
    }
  }

  def validUnsignedErgoTransactionGenTemplate(minAssets: Int,
                                              maxAssets: Int = -1,
                                              minInputs: Int = 1,
                                              maxInputs: Int = 100,
                                              propositionGen: Gen[ErgoTree] = trueLeafGen
                                             ): Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] = {
    boxesGenTemplate(minAssets, maxAssets, maxInputs, maxInputs, propositionGen).map { case (boxes, prop) =>
      val utx = validUnsignedTransactionFromBoxes(boxes, outputsProposition = prop)
      boxes -> utx
    }
  }

  def validUnsignedErgoTransactionGen(prop: ErgoTree*): Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] =
    validUnsignedErgoTransactionGenTemplate(0, maxAssets = 5, maxInputs = 10, propositionGen = Gen.oneOf(prop))

  lazy val validUnsignedErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] =
    validUnsignedErgoTransactionGenTemplate(0, maxAssets = 5, maxInputs = 10)

  lazy val validErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = validErgoTransactionGenTemplate(0)
  lazy val validErgoTransactionWithAssetsGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] =
    validErgoTransactionGenTemplate(1)

  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGenForTokens(Seq(), trueLeafGen))
    .map(l => BoxHolder(l))

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.nonEmptyListOf(invalidErgoTransactionGen)
  } yield BlockTransactions(headerId, Header.InitialVersion, txs.foldLeft(Seq.empty[ErgoTransaction])((acc, tx) =>
    if ((acc :+ tx).map(_.size).sum < (Parameters.MaxBlockSizeDefault - 150)) acc :+ tx else acc))

  def invalidBlockTransactionsGen(prop: ProveDlog, txQty: Int): Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.listOfN(txQty, invalidErgoTransactionGen(prop))
  } yield BlockTransactions(headerId, Header.InitialVersion, txs.foldLeft(Seq.empty[ErgoTransaction])((acc, tx) =>
    if ((acc :+ tx).map(_.size).sum < (Parameters.MaxBlockSizeDefault - 150)) acc :+ tx else acc))

  lazy val randomUTXOSnapshotChunkGen: Gen[UTXOSnapshotChunk] = for {
    index: Short <- Arbitrary.arbitrary[Short]
    stateElements: Seq[ErgoBox] <- Gen.listOf(ergoBoxGenNoProp)
  } yield UTXOSnapshotChunk(stateElements, index)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- defaultHeaderGen
    txs <- invalidBlockTransactionsGen
    extension <- extensionGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, extension, Some(proof))

  def invalidErgoFullBlockGen(prop: ProveDlog, txQty: Int): Gen[ErgoFullBlock] = for {
    header <- defaultHeaderGen
    txs <- invalidBlockTransactionsGen(prop, txQty)
    extension <- extensionGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, extension, Some(proof))

  lazy val paramVoteGen: Gen[Byte] = for {
    paramVote <- Gen.oneOf(Seq(NoParameter, StorageFeeFactorIncrease, MinValuePerByteIncrease))
  } yield paramVote

  lazy val paramVotesGen: Gen[Array[Byte]] = for {
    firstVote <- paramVoteGen
  } yield Array(firstVote, NoParameter, NoParameter)

  lazy val ergoStateContextGen: Gen[ErgoStateContext] = for {
    size <- Gen.choose(0, Constants.LastHeadersInContext + 3)
    stateRoot <- stateRootGen
    blocks <- Gen.listOfN(size, invalidErgoFullBlockGen)
    votes <- Gen.listOfN(size, paramVotesGen)
  } yield {
    blocks match {
      case _ :: _ =>
        val sc = new ErgoStateContext(Seq(), None, startDigest, parameters, validationSettingsNoIl, VotingData.empty)
        blocks.foldLeft(sc -> 1) { case ((c, h), b) =>
          val block = b.copy(header = b.header.copy(height = h, votes = votes(h - 1)))
          c.appendFullBlock(block).get -> (h + 1)
        }._1
      case _ =>
        ErgoStateContext.empty(stateRoot, settings)
    }
  }

  def transactionSigningRequestGen(includeInputs: Boolean): Gen[TransactionSigningRequest] = for {
    (secret, pubKey) <- dlogSecretWithPublicImageGen
    (secretDh, _) <- dhtSecretWithPublicImageGen
    (inputBoxes, utx) <- validUnsignedErgoTransactionGen(pubKey)
    inputBoxesEncoded = inputBoxes.map(b => Base16.encode(b.bytes))
    secretSeq = Seq(ExternalSecret(DlogSecretKey(secret)), ExternalSecret(DhtSecretKey(secretDh)))
  } yield TransactionSigningRequest(utx, TransactionHintsBag.empty, secretSeq,
    if (includeInputs) Some(inputBoxesEncoded) else None, None)

  def transactionSigningRequestGen(utxoSet: WrappedUtxoState): Gen[TransactionSigningRequest] = Gen.const {
    val inputBoxes = utxoSet.takeBoxes(3).toIndexedSeq

    val utx = UnsignedErgoTransaction(inputBoxes.map(b => new UnsignedInput(b.id)), inputBoxes)
    val coin = Random.nextBoolean()
    val inputBoxesEncoded = inputBoxes.map(b => Base16.encode(b.bytes))

    TransactionSigningRequest(utx, TransactionHintsBag.empty, Seq.empty, if (coin) Some(inputBoxesEncoded) else None, None)
  }

}

object ErgoTransactionGenerators extends ErgoTransactionGenerators
