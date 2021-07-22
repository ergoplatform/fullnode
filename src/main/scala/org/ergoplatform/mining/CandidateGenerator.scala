package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import akka.util.Timeout
import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.mining.AutolykosPowScheme.derivedHeaderFields
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{
  ErgoState,
  ErgoStateContext,
  StateType,
  UtxoStateReader
}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, Input}
import scorex.core.NodeViewHolder.ReceivableMessages.{
  EliminateTransactions,
  LocallyGeneratedModifier
}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.utils.NetworkTimeProvider
import scorex.core.validation.ValidationSettings
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.SType.ErgoBoxRType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.ProverResult
import special.collection.Coll

import scala.annotation.tailrec
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/** Responsible for generating block candidates and validating solutions.
  * It is observing changes of history, utxo state, mempool and newly applied blocks
  * to generate valid block candidates when it is needed. */
class CandidateGenerator(
  minerPk: ProveDlog,
  readersHolderRef: ActorRef,
  viewHolderRef: ActorRef,
  timeProvider: NetworkTimeProvider,
  ergoSettings: ErgoSettings
) extends Actor
  with ScorexLogging {

  import org.ergoplatform.mining.CandidateGenerator._

  implicit private val dispatcher: ExecutionContextExecutor = context.system.dispatcher
  implicit private val timeout: Timeout                     = 5.seconds

  /** retrieve Readers once on start and then get updated by events */
  override def preStart(): Unit = {
    log.info("CandidateGenerator is starting")
    readersHolderRef ! GetReaders
  }

  /** checks that current candidate block is cached with given `txs` */
  private def cachedFor(
    candidateOpt: Option[Candidate],
    txs: Seq[ErgoTransaction]
  ): Boolean = {
    candidateOpt.isDefined && candidateOpt.exists { c =>
      txs.isEmpty || (txs.size == c.txsToInclude.size && txs.forall(
        c.txsToInclude.contains
      ))
    }
  }

  /** we need new candidate if given block is not parent of our cached block */
  private def needNewCandidate(
    cache: Option[Candidate],
    b: ErgoFullBlock
  ): Boolean = {
    val parentHeaderIdOpt = cache.map(_.candidateBlock).flatMap(_.parentOpt).map(_.id)
    !parentHeaderIdOpt.contains(b.header.id)
  }

  /** clear solution and cached block candidate and generate fresh one */
  private def restartState(state: CandidateGeneratorState): Unit = {
    context.become(initialized(state.copy(cache = None, solvedBlock = None)))
    self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false)
  }

  /** Send solved block to local blockchain controller */
  private def sendToNodeView(newBlock: ErgoFullBlock): Unit = {
    log.info(
      s"New block ${newBlock.id} w. nonce ${Longs.fromByteArray(newBlock.header.powSolution.n)}"
    )
    viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
    val sectionsToApply = if (ergoSettings.nodeSettings.stateType == StateType.Digest) {
      newBlock.blockSections
    } else {
      newBlock.mandatoryBlockSections
    }
    sectionsToApply.foreach(viewHolderRef ! LocallyGeneratedModifier(_))
  }

  override def receive: Receive = {
    /** first we need to get Readers to have some initial state to work with */
    case Readers(h, s: UtxoStateReader, m, _) =>
      log.info("CandidateGenerator successfully initialized")
      context.become(
        initialized(
          CandidateGeneratorState(
            cache       = None,
            solvedBlock = None,
            h,
            s,
            m,
            avgGenTime = 1000.millis
          )
        )
      )
      self ! GenerateCandidate(txsToInclude = Seq.empty, reply = false)
      context.system.eventStream
        .subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
      context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    case Readers(_, _, _, _) =>
      log.error("Invalid readers state, mining is possible in UTXO mode only")
    case gen @ GenerateCandidate(_, _) =>
      // retry until initialized
      context.system.scheduler
        .scheduleOnce(100.millis, self, gen)(context.dispatcher, sender())
  }

  private def initialized(state: CandidateGeneratorState): Receive = {
    case ChangedHistory(h: ErgoHistoryReader) =>
      context.become(initialized(state.copy(hr = h)))
    case ChangedState(s: UtxoStateReader) =>
      context.become(initialized(state.copy(sr = s)))
    case ChangedMempool(mp: ErgoMemPoolReader) =>
      context.become(initialized(state.copy(mpr = mp)))
    case _: NodeViewChange =>
    // Just ignore all other NodeView Changes

    /**
      * Case when we are already mining by the time modifier arrives and
      * get block from node view that has header's id which isn't equals to our candidate's parent id.
      * That means that our candidate is outdated. Should produce new candidate for ourselves.
      * Stop all current threads and re-run them with newly produced candidate.
      */
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock)
        if needNewCandidate(state.cache, mod) =>
      log.info(s"Preparing new candidate on getting new block at ${mod.height}")
      if (state.solvedBlock.nonEmpty && (state.solvedBlock.map(_.parentId) != state.hr.bestFullBlockOpt
            .map(_.id)))
        restartState(state)

    case SemanticallySuccessfulModifier(_) =>
    // Just ignore all other modifiers.

    case gen @ GenerateCandidate(txsToInclude, reply) =>
      val senderOpt = if (reply) Some(sender()) else None
      if (cachedFor(state.cache, txsToInclude)) {
        senderOpt.foreach(_ ! StatusReply.success(state.cache.get))
      } else {
        val start = System.currentTimeMillis()
        CandidateGenerator.generateCandidate(
          state.hr,
          state.sr,
          state.mpr,
          timeProvider,
          minerPk,
          txsToInclude,
          ergoSettings
        ) match {
          case Some(Failure(ex)) =>
            log.error(s"Candidate generation failed", ex)
            senderOpt.foreach(
              _ ! StatusReply.error(s"Candidate generation failed : ${ex.getMessage}")
            )
          case Some(Success((candidate, eliminatedTxs))) =>
            if (eliminatedTxs.ids.nonEmpty) viewHolderRef ! eliminatedTxs
            val generationTook = System.currentTimeMillis() - start
            log.info(s"Generated new candidate in $generationTook ms")
            context.become(
              initialized(
                state.copy(cache = Some(candidate), avgGenTime = generationTook.millis)
              )
            )
            senderOpt.foreach(_ ! StatusReply.success(candidate))
          case None =>
            log.warn(
              "Can not generate block candidate: either mempool is empty or chain is not synced (maybe last block not fully applied yet"
            )
            senderOpt.foreach { s =>
              context.system.scheduler.scheduleOnce(state.avgGenTime, self, gen)(
                context.system.dispatcher,
                s
              )
            }
        }
      }

    case preSolution: AutolykosSolution
        if state.solvedBlock.isEmpty && state.cache.nonEmpty =>
      // Inject node pk if it is not externally set (in Autolykos 2)
      val solution =
        if (preSolution.pk.isInfinity) {
          AutolykosSolution(minerPk.value, preSolution.w, preSolution.n, preSolution.d)
        } else {
          preSolution
        }
      val result: StatusReply[Unit] = {
        val newBlock = completeBlock(state.cache.get.candidateBlock, solution)
        log.info(s"New block mined, header: ${newBlock.header}")
        ergoSettings.chainSettings.powScheme
          .validate(newBlock.header)
          .map(_ => newBlock) match {
          case Success(newBlock) =>
            sendToNodeView(newBlock)
            context.become(initialized(state.copy(solvedBlock = Some(newBlock))))
            StatusReply.success(())
          case Failure(exception) =>
            log.warn(s"Removing candidate due to invalid block", exception)
            context.become(initialized(state.copy(cache = None)))
            StatusReply.error(
              new Exception(s"Invalid block mined: ${exception.getMessage}", exception)
            )
        }
      }
      log.info(s"Processed solution $solution with the result $result")
      sender() ! result

    case _: AutolykosSolution =>
      sender() ! StatusReply.error(
        s"Block already solved : ${state.solvedBlock.map(_.id)}"
      )

  }

}

object CandidateGenerator extends ScorexLogging {

  /**
    * Holder for both candidate block and data for external miners derived from it
    * (to avoid possibly costly recalculation)
    *
    * @param candidateBlock  - block candidate
    * @param externalVersion - message for external miner
    * @param txsToInclude    - transactions which were prioritized for inclusion in the block candidate
    */
  case class Candidate(
    candidateBlock: CandidateBlock,
    externalVersion: WorkMessage,
    txsToInclude: Seq[ErgoTransaction]
  )

  case class GenerateCandidate(
    txsToInclude: Seq[ErgoTransaction],
    reply: Boolean
  )

  /** Local state of candidate generator to avoid mutable vars */
  case class CandidateGeneratorState(
    cache: Option[Candidate],
    solvedBlock: Option[ErgoFullBlock],
    hr: ErgoHistoryReader,
    sr: UtxoStateReader,
    mpr: ErgoMemPoolReader,
    avgGenTime: FiniteDuration // approximation of average block generation time for more efficient retries
  )

  def apply(
    minerPk: ProveDlog,
    readersHolderRef: ActorRef,
    viewHolderRef: ActorRef,
    timeProvider: NetworkTimeProvider,
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ActorRef =
    context.actorOf(
      Props(
        new CandidateGenerator(
          minerPk,
          readersHolderRef,
          viewHolderRef,
          timeProvider,
          ergoSettings
        )
      ),
      s"CandidateGenerator-${Random.alphanumeric.take(5).mkString}"
    )

  // helper which is checking that inputs of the transaction are not spent
  private def inputsNotSpent(tx: ErgoTransaction, s: UtxoStateReader): Boolean =
    tx.inputs.forall(inp => s.boxById(inp.boxId).isDefined)

  /**
    * @return None if chain is not synced or Some of attempt to create candidate
    */
  def generateCandidate(
    h: ErgoHistoryReader,
    s: UtxoStateReader,
    m: ErgoMemPoolReader,
    timeProvider: NetworkTimeProvider,
    pk: ProveDlog,
    txsToInclude: Seq[ErgoTransaction],
    ergoSettings: ErgoSettings
  ): Option[Try[(Candidate, EliminateTransactions)]] = {
    //mandatory transactions to include into next block taken from the previous candidate
    lazy val unspentTxsToInclude = txsToInclude.filter { tx =>
      inputsNotSpent(tx, s)
    }

    //only transactions valid from against the current utxo state we take from the mem pool
    lazy val poolTransactions = m.getAllPrioritized

    lazy val emissionTxOpt =
      CandidateGenerator.collectEmission(s, pk, ergoSettings.chainSettings.emissionRules)

    def chainSynced =
      h.bestFullBlockOpt.map(_.id) == s.stateContext.lastHeaderOpt.map(_.id)

    def hasAnyMemPoolOrMinerTx =
      poolTransactions.nonEmpty || unspentTxsToInclude.nonEmpty || emissionTxOpt.nonEmpty

    if (!hasAnyMemPoolOrMinerTx) {
      log.info(s"Avoiding generation of a block without any transactions")
      None
    } else if (!chainSynced) {
      log.info(
        "Chain not synced probably due to racing condition when last block is not fully applied yet"
      )
      None
    } else {
      val desiredUpdate = ergoSettings.votingTargets.desiredUpdate
      Some(
        createCandidate(
          pk,
          h,
          desiredUpdate,
          s,
          timeProvider,
          poolTransactions,
          emissionTxOpt,
          unspentTxsToInclude,
          ergoSettings
        )
      )
    }
  }

  /**
    * Assemble correct block candidate based on
    *
    * @param minerPk                 - public key of the miner
    * @param history                 - blockchain reader (to extract parent)
    * @param proposedUpdate          - votes for parameters update or/and soft-fork
    * @param state                   - UTXO set reader
    * @param timeProvider            - network time provider
    * @param poolTxs                 - memory pool transactions
    * @param emissionTxOpt           - optional emission transaction
    * @param prioritizedTransactions - transactions which are going into the block in the first place
    *                                (before transactions from the pool). No guarantee of inclusion in general case.
    * @return - candidate or an error
    */
  def createCandidate(
    minerPk: ProveDlog,
    history: ErgoHistoryReader,
    proposedUpdate: ErgoValidationSettingsUpdate,
    state: UtxoStateReader,
    timeProvider: NetworkTimeProvider,
    poolTxs: Seq[ErgoTransaction],
    emissionTxOpt: Option[ErgoTransaction],
    prioritizedTransactions: Seq[ErgoTransaction],
    ergoSettings: ErgoSettings
  ): Try[(Candidate, EliminateTransactions)] =
    Try {
      val popowAlgos = new NipopowAlgos(ergoSettings.chainSettings.powScheme)
      // Extract best header and extension of a best block user their data for assembling a new block
      val bestHeaderOpt: Option[Header] = history.bestFullBlockOpt.map(_.header)
      val bestExtensionOpt: Option[Extension] = bestHeaderOpt
        .flatMap(h => history.typedModifierById[Extension](h.extensionId))

      // Make progress in time since last block.
      // If no progress is made, then, by consensus rules, the block will be rejected.
      val timestamp =
        Math.max(timeProvider.time(), bestHeaderOpt.map(_.timestamp + 1).getOrElse(0L))

      val stateContext = state.stateContext

      // Calculate required difficulty for the new block
      val nBits: Long = bestHeaderOpt
        .map(parent => history.requiredDifficultyAfter(parent))
        .map(d => RequiredDifficulty.encodeCompactBits(d))
        .getOrElse(ergoSettings.chainSettings.initialNBits)

      // Obtain NiPoPoW interlinks vector to pack it into the extension section
      val updInterlinks       = popowAlgos.updateInterlinks(bestHeaderOpt, bestExtensionOpt)
      val interlinksExtension = popowAlgos.interlinksToExtension(updInterlinks)
      val votingSettings      = ergoSettings.chainSettings.voting
      val (extensionCandidate, votes: Array[Byte], version: Byte) = bestHeaderOpt
        .map { header =>
          val newHeight     = header.height + 1
          val currentParams = stateContext.currentParameters
          val betterVersion = ergoSettings.chainSettings.protocolVersion > header.version
          val votingFinishHeight: Option[Height] = currentParams.softForkStartingHeight
            .map(_ + votingSettings.votingLength * votingSettings.softForkEpochs)
          val forkVotingAllowed = votingFinishHeight.forall(fh => newHeight < fh)
          val forkOrdered       = ergoSettings.votingTargets.softFork != 0
          val voteForFork       = betterVersion && forkOrdered && forkVotingAllowed

          if (newHeight % votingSettings.votingLength == 0 && newHeight > 0) {
            val (newParams, activatedUpdate) = currentParams.update(
              newHeight,
              voteForFork,
              stateContext.votingData.epochVotes,
              proposedUpdate,
              votingSettings
            )
            val newValidationSettings =
              stateContext.validationSettings.updated(activatedUpdate)
            (
              newParams.toExtensionCandidate ++ interlinksExtension ++ newValidationSettings.toExtensionCandidate,
              newParams.suggestVotes(ergoSettings.votingTargets.targets, voteForFork),
              newParams.blockVersion
            )
          } else {
            (
              interlinksExtension,
              currentParams.vote(
                ergoSettings.votingTargets.targets,
                stateContext.votingData.epochVotes,
                voteForFork
              ),
              currentParams.blockVersion
            )
          }
        }
        .getOrElse(
          (interlinksExtension, Array(0: Byte, 0: Byte, 0: Byte), Header.InitialVersion)
        )

      val upcomingContext = state.stateContext.upcoming(
        minerPk.h,
        timestamp,
        nBits,
        votes,
        proposedUpdate,
        version
      )

      val emissionTxs = emissionTxOpt.toSeq
      val (txs, toEliminate) = collectTxs(
        minerPk,
        state.stateContext.currentParameters.maxBlockCost,
        state.stateContext.currentParameters.maxBlockSize,
        ergoSettings.nodeSettings.maxTransactionComplexity,
        state,
        upcomingContext,
        emissionTxs ++ prioritizedTransactions ++ poolTxs
      )(state.stateContext.validationSettings)

      val eliminateTransactions = EliminateTransactions(toEliminate)

      if (txs.isEmpty) {
        throw new IllegalArgumentException(
          s"Proofs for 0 txs cannot be generated : emissionTxs: ${emissionTxs.size}, priorityTxs: ${prioritizedTransactions.size}, poolTxs: ${poolTxs.size}"
        )
      }

      def deriveWorkMessage(block: CandidateBlock) =
        ergoSettings.chainSettings.powScheme.deriveExternalCandidate(
          block,
          minerPk,
          prioritizedTransactions.map(_.id)
        )

      state.proofsForTransactions(txs) match {
        case Success((adProof, adDigest)) =>
          val candidate = CandidateBlock(
            bestHeaderOpt,
            version,
            nBits,
            adDigest,
            adProof,
            txs,
            timestamp,
            extensionCandidate,
            votes
          )
          val ext = deriveWorkMessage(candidate)
          log.info(s"New candidate with msg ${Base16.encode(ext.msg)} generated")
          log.debug(
            s"Got candidate block at height ${ErgoHistory.heightOf(candidate.parentOpt) + 1}" +
            s" with ${candidate.transactions.size} transactions"
          )
          Success(
            Candidate(candidate, ext, prioritizedTransactions) -> eliminateTransactions
          )
        case Failure(t: Throwable) =>
          // We can not produce a block for some reason, so print out an error
          // and collect only emission transaction if it exists.
          // We consider that emission transaction is always valid.
          emissionTxOpt match {
            case Some(emissionTx) =>
              log.error(
                "Failed to produce proofs for transactions, but emission box is found: ",
                t
              )
              val fallbackTxs = Seq(emissionTx)
              state.proofsForTransactions(fallbackTxs).map {
                case (adProof, adDigest) =>
                  val candidate = CandidateBlock(
                    bestHeaderOpt,
                    version,
                    nBits,
                    adDigest,
                    adProof,
                    fallbackTxs,
                    timestamp,
                    extensionCandidate,
                    votes
                  )
                  Candidate(
                    candidate,
                    deriveWorkMessage(candidate),
                    prioritizedTransactions
                  ) -> eliminateTransactions
              }
            case None =>
              log.error(
                "Failed to produce proofs for transactions and no emission box available: ",
                t
              )
              Failure(t)
          }
      }
    }.flatten

  /**
    * Transaction and its cost.
    */
  type CostedTransaction = (ErgoTransaction, Long)

  //TODO move ErgoMiner to mining package and make `collectTxs` and `fixTxsConflicts` private[mining]

  def collectEmission(
    state: UtxoStateReader,
    minerPk: ProveDlog,
    emission: EmissionRules
  ): Option[ErgoTransaction] = {
    collectRewards(
      state.emissionBoxOpt,
      state.stateContext.currentHeight,
      Seq.empty,
      minerPk,
      emission,
      Colls.emptyColl
    ).headOption
  }

  def collectFees(
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    emission: EmissionRules
  ): Option[ErgoTransaction] = {
    collectRewards(None, currentHeight, txs, minerPk, emission, Colls.emptyColl).headOption
  }

  /**
    * Generate from 0 to 2 transaction that collecting rewards from fee boxes in block transactions `txs` and
    * emission box `emissionBoxOpt`
    */
  def collectRewards(
    emissionBoxOpt: Option[ErgoBox],
    currentHeight: Int,
    txs: Seq[ErgoTransaction],
    minerPk: ProveDlog,
    emission: EmissionRules,
    assets: Coll[(TokenId, Long)] = Colls.emptyColl
  ): Seq[ErgoTransaction] = {
    val propositionBytes = emission.settings.feePropositionBytes

    val inputs = txs.flatMap(_.inputs)
    val feeBoxes: Seq[ErgoBox] = ErgoState
      .boxChanges(txs)
      ._2
      .filter(b => java.util.Arrays.equals(b.propositionBytes, propositionBytes))
      .filter(b => !inputs.exists(i => java.util.Arrays.equals(i.boxId, b.id)))
    val nextHeight = currentHeight + 1
    val minerProp =
      ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, minerPk)

    val emissionTxOpt: Option[ErgoTransaction] = emissionBoxOpt.map { emissionBox =>
      val prop           = emissionBox.ergoTree
      val emissionAmount = emission.minersRewardAtHeight(nextHeight)
      val newEmissionBox: ErgoBoxCandidate =
        new ErgoBoxCandidate(emissionBox.value - emissionAmount, prop, nextHeight)
      val inputs = IndexedSeq(new Input(emissionBox.id, ProverResult.empty))

      val minerBox = new ErgoBoxCandidate(emissionAmount, minerProp, nextHeight, assets)

      ErgoTransaction(
        inputs,
        IndexedSeq(),
        IndexedSeq(newEmissionBox, minerBox)
      )
    }
    val feeTxOpt: Option[ErgoTransaction] = if (feeBoxes.nonEmpty) {
      val feeAmount = feeBoxes.map(_.value).sum
      val feeAssets =
        feeBoxes.toColl.flatMap(_.additionalTokens).take(ErgoBox.MaxTokens - 1)
      val inputs = feeBoxes.map(b => new Input(b.id, ProverResult.empty))
      val minerBox =
        new ErgoBoxCandidate(feeAmount, minerProp, nextHeight, feeAssets, Map())
      Some(ErgoTransaction(inputs.toIndexedSeq, IndexedSeq(), IndexedSeq(minerBox)))
    } else {
      None
    }
    Seq(emissionTxOpt, feeTxOpt).flatten
  }

  /**
    * Helper function which decides whether transactions can fit into a block with given cost and size limits
    */
  def correctLimits(
    blockTxs: Seq[CostedTransaction],
    maxBlockCost: Long,
    maxBlockSize: Long
  ): Boolean = {
    blockTxs.map(_._2).sum < maxBlockCost && blockTxs.map(_._1.size).sum < maxBlockSize
  }

  /**
    * Collects valid non-conflicting transactions from `mandatoryTxs` and then `mempoolTxsIn` and adds a transaction
    * collecting fees from them to `minerPk`.
    *
    * Resulting transactions total cost does not exceed `maxBlockCost`, total size does not exceed `maxBlockSize`,
    * and the miner's transaction is correct.
    *
    * @return - transactions to include into the block, transaction ids turned out to be invalid.
    */
  def collectTxs(
    minerPk: ProveDlog,
    maxBlockCost: Long,
    maxBlockSize: Long,
    maxTransactionComplexity: Int,
    us: UtxoStateReader,
    upcomingContext: ErgoStateContext,
    transactions: Seq[ErgoTransaction]
  )(implicit vs: ValidationSettings): (Seq[ErgoTransaction], Seq[ModifierId]) = {

    val currentHeight = us.stateContext.currentHeight

    log.info(
      s"Assembling a block candidate for block #$currentHeight from ${transactions.length} transactions available"
    )

    @tailrec
    def loop(
      mempoolTxs: Iterable[ErgoTransaction],
      acc: Seq[CostedTransaction],
      lastFeeTx: Option[CostedTransaction],
      invalidTxs: Seq[ModifierId]
    ): (Seq[ErgoTransaction], Seq[ModifierId]) = {
      // transactions from mempool and fee txs from the previous step
      def current: Seq[ErgoTransaction] = (acc ++ lastFeeTx).map(_._1)

      val stateWithTxs = us.withTransactions(current)

      mempoolTxs.headOption match {
        case Some(tx) =>
          if (!inputsNotSpent(tx, stateWithTxs) || doublespend(current, tx)) {
            //mark transaction as invalid if it tries to do double-spending or trying to spend outputs not present
            //do these checks before validating the scripts to save time
            loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
          } else {
            implicit val verifier: ErgoInterpreter = ErgoInterpreter(
              us.stateContext.currentParameters
            )
            // check validity and calculate transaction cost
            stateWithTxs.validateWithCost(
              tx,
              Some(upcomingContext),
              maxTransactionComplexity
            ) match {
              case Success(costConsumed) =>
                val newTxs   = acc :+ (tx -> costConsumed)
                val newBoxes = newTxs.flatMap(_._1.outputs)

                val emissionRules =
                  stateWithTxs.constants.settings.chainSettings.emissionRules
                collectFees(currentHeight, newTxs.map(_._1), minerPk, emissionRules) match {
                  case Some(feeTx) =>
                    val boxesToSpend = feeTx.inputs.flatMap(i =>
                      newBoxes.find(b => java.util.Arrays.equals(b.id, i.boxId))
                    )
                    feeTx.statefulValidity(boxesToSpend, IndexedSeq(), upcomingContext) match {
                      case Success(cost) =>
                        val blockTxs: Seq[CostedTransaction] = (feeTx -> cost) +: newTxs
                        if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                          loop(mempoolTxs.tail, newTxs, Some(feeTx -> cost), invalidTxs)
                        } else {
                          current -> invalidTxs
                        }
                      case Failure(e) =>
                        log.warn(
                          s"Fee collecting tx is invalid, not including it, " +
                          s"details: ${e.getMessage} from ${stateWithTxs.stateContext}"
                        )
                        current -> invalidTxs
                    }
                  case None =>
                    log.info(s"No fee proposition found in txs ${newTxs.map(_._1.id)} ")
                    val blockTxs: Seq[CostedTransaction] = newTxs ++ lastFeeTx.toSeq
                    if (correctLimits(blockTxs, maxBlockCost, maxBlockSize)) {
                      loop(mempoolTxs.tail, blockTxs, lastFeeTx, invalidTxs)
                    } else {
                      current -> invalidTxs
                    }
                }
              case Failure(e) =>
                log.debug(s"Not included transaction ${tx.id} due to ${e.getMessage}")
                loop(mempoolTxs.tail, acc, lastFeeTx, invalidTxs :+ tx.id)
            }
          }
        case _ => // mempool is empty
          current -> invalidTxs
      }
    }

    val res = loop(transactions, Seq.empty, None, Seq.empty)
    log.info(
      s"Collected ${res._1.length} transactions For block #$currentHeight, " +
      s"${res._2.length} transactions turned out to be invalid"
    )
    res
  }

  /** Checks that transaction "tx" is not spending outputs spent already by transactions "txs" */
  def doublespend(txs: Seq[ErgoTransaction], tx: ErgoTransaction): Boolean = {
    val txsInputs = txs.flatMap(_.inputs.map(_.boxId))
    tx.inputs.exists(i => txsInputs.exists(_.sameElements(i.boxId)))
  }

  /**
    * Derives header without pow from [[CandidateBlock]].
    */
  def deriveUnprovenHeader(candidate: CandidateBlock): HeaderWithoutPow = {
    val (parentId, height) = derivedHeaderFields(candidate.parentOpt)
    val transactionsRoot =
      BlockTransactions.transactionsRoot(candidate.transactions, candidate.version)
    val adProofsRoot            = ADProofs.proofDigest(candidate.adProofBytes)
    val extensionRoot: Digest32 = candidate.extension.digest

    HeaderWithoutPow(
      candidate.version,
      parentId,
      adProofsRoot,
      candidate.stateRoot,
      transactionsRoot,
      candidate.timestamp,
      candidate.nBits,
      height,
      extensionRoot,
      candidate.votes
    )
  }

  /**
    * Assemble `ErgoFullBlock` using candidate block and provided pow solution.
    */
  def completeBlock(
    candidate: CandidateBlock,
    solution: AutolykosSolution
  ): ErgoFullBlock = {
    val header   = deriveUnprovenHeader(candidate).toHeader(solution, None)
    val adProofs = ADProofs(header.id, candidate.adProofBytes)
    val blockTransactions =
      BlockTransactions(header.id, candidate.version, candidate.transactions)
    val extension = Extension(header.id, candidate.extension.fields)
    new ErgoFullBlock(header, blockTransactions, extension, Some(adProofs))
  }

}
