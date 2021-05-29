package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.ExtensionValidator
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{Constants, _}
import org.ergoplatform.wallet.protocol.context.ErgoLikeStateContext
import scorex.core.block.Block
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationState}
import scorex.crypto.authds.ADDigest
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollOverArray}

import scala.util.{Failure, Success, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
case class UpcomingStateContext(override val lastHeaders: Seq[Header],
                                override val lastExtensionOpt: Option[Extension],
                                predictedHeader: PreHeader,
                                override val genesisStateDigest: ADDigest,
                                override val currentParameters: Parameters,
                                override val validationSettings: ErgoValidationSettings,
                                override val votingData: VotingData)(implicit ergoSettings: ErgoSettings)
  extends ErgoStateContext(lastHeaders, lastExtensionOpt, genesisStateDigest, currentParameters, validationSettings, votingData)(ergoSettings) {

  override def sigmaPreHeader: special.sigma.PreHeader = PreHeader.toSigma(predictedHeader)

  override def sigmaLastHeaders: Coll[special.sigma.Header] = new CollOverArray(lastHeaders.map(h => Header.toSigma(h)).toArray)

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"

}

/**
  * Additional data required for transactions validation.
  * Script validation requires that it contain at least a preHeader, so it can only be used
  * for transaction validation if lastHeaders not empty or in `upcoming` version.
  *
  * @param lastHeaders        - fixed number (10) of last headers
  * @param lastExtensionOpt   - last block extension
  * @param genesisStateDigest - genesis state digest (before the very first block)
  * @param currentParameters  - parameters at the beginning of the current voting epoch
  * @param votingData         - votes for parameters change within the current voting epoch
  */
class ErgoStateContext(val lastHeaders: Seq[Header],
                       val lastExtensionOpt: Option[Extension],
                       val genesisStateDigest: ADDigest,
                       val currentParameters: Parameters,
                       val validationSettings: ErgoValidationSettings,
                       val votingData: VotingData)
                      (implicit val ergoSettings: ErgoSettings)
  extends ErgoLikeStateContext
    with BytesSerializable
    with ScorexEncoding
    with ScorexLogging {

  override type M = ErgoStateContext

  private val votingSettings = ergoSettings.chainSettings.voting
  private val powScheme = ergoSettings.chainSettings.powScheme
  private val popowAlgos = new NipopowAlgos(powScheme)

  override def sigmaPreHeader: special.sigma.PreHeader =
    PreHeader.toSigma(lastHeaders.headOption.getOrElse(PreHeader.fake))

  override def sigmaLastHeaders: Coll[special.sigma.Header] = new CollOverArray(lastHeaders.drop(1).map(h => Header.toSigma(h)).toArray)

  // todo remove from ErgoLikeContext and from ErgoStateContext
  // State root hash before the last block
  override def previousStateDigest: ADDigest = if (sigmaLastHeaders.toArray.nonEmpty) {
    ADDigest @@ sigmaLastHeaders.toArray.head.stateRoot.digest.toArray
  } else {
    genesisStateDigest
  }

  /* NOHF PROOF:
  Changed: removed returning ErgoHistory.EmptyHistoryHeight if sigmaPreHeader is null.
  Motivation: sigmaPreHeader no longer can be null, returning PreHeader.fake.
  Safety: In PreHeader.fake.height = ErgoHistory.EmptyHistoryHeight.
  */
  // todo remove from ErgoLikeContext and from ErgoStateContext
  def currentHeight: Int = sigmaPreHeader.height

  /**
    * @return block version of the protocol
    */
  def blockVersion: Block.Version = currentParameters.blockVersion

  private def votingEpochLength: Int = votingSettings.votingLength

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  override def serializer: ScorexSerializer[M] = ErgoStateContextSerializer(ergoSettings)

  def upcoming(minerPk: EcPointType,
               timestamp: Long,
               nBits: Long,
               votes: Array[Byte],
               proposedUpdate: ErgoValidationSettingsUpdate,
               version: Byte): UpcomingStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes)
    val forkVote = votes.contains(Parameters.SoftFork)
    val height = ErgoHistory.heightOf(lastHeaderOpt) + 1
    val (calculatedParams, updated) = currentParameters.update(height, forkVote, votingData.epochVotes, proposedUpdate, votingSettings)
    val calculatedValidationSettings = validationSettings.updated(updated)
    UpcomingStateContext(lastHeaders, lastExtensionOpt, upcomingHeader, genesisStateDigest, calculatedParams, calculatedValidationSettings, votingData)
  }

  protected def checkForkVote(height: Height): Unit = {
    if (currentParameters.softForkStartingHeight.nonEmpty) {
      val startingHeight = currentParameters.softForkStartingHeight.get
      val finishingHeight = startingHeight + votingSettings.votingLength * votingSettings.softForkEpochs
      val afterActivationHeight = finishingHeight + votingSettings.votingLength * (votingSettings.activationEpochs + 1)
      val votesCollected = currentParameters.softForkVotesCollected.get

      if ((height >= finishingHeight && height < finishingHeight + votingEpochLength && !votingSettings.softForkApproved(votesCollected)) ||
        (height >= finishingHeight && height < afterActivationHeight && votingSettings.softForkApproved(votesCollected))) {
        throw new Exception(s"Voting for fork is prohibited at height $height")
      }
    }
  }

  /**
    * Called at the beginning of the epoch.
    * Extracts parameters and validationSettings from `Extension` and compares them to locally calculated once.
    */
  private def processExtension(extension: Extension, header: Header, forkVote: Boolean)
                              (validationState: ValidationState[Unit]): Try[(Parameters, ErgoValidationSettings)] = {
    val height = header.height
    val parsedParamsTry = Parameters.parseExtension(height, extension)
    val parsedValidationSettingsTry = ErgoValidationSettings.parseExtension(extension)

    validationState
      .validateNoFailure(exParseParameters, parsedParamsTry)
      .validateNoFailure(exParseValidationSettings, parsedValidationSettingsTry)
      .validateTry(parsedParamsTry, e => ModifierValidator.fatal("Failed to parse parameters", e)) {
        case (vs, parsedParams) =>
          vs.validateTry(parsedValidationSettingsTry, e => ModifierValidator.fatal("Failed to parse validation settings", e)) {
            case (currentValidationState, parsedSettings) =>

              /*
               Calculating blockchain parameters and validation rules based on the locally stored blockchain,
               to compare them then with announced ones.
               If current parameters height is equal to 0, it means that whether it is first epoch after genesis,
               or node is starting in light regime with validating suffix only. For the former case, we assume that
               parameters and validation settings were not changed, which is true for both Ergo mainnet and testnet.
               For the latter case, the light fullnode is just relied on PoW majority about parameters and validation
               settings.
               */
              val (calculatedParams, calculatedSettings) = if (currentParameters.height == 0) {
                parsedParams -> parsedSettings
              } else {
                val (params, settingsUpdates) = currentParameters
                  .update(height, forkVote, votingData.epochVotes, parsedParams.proposedUpdate, votingSettings)
                val settings = validationSettings.updated(settingsUpdates)
                params -> settings
              }

              currentValidationState
                .validate(exBlockVersion, calculatedParams.blockVersion == header.version, s"${calculatedParams.blockVersion} == ${header.version}")
                .validateNoFailure(exMatchParameters, Parameters.matchParameters(parsedParams, calculatedParams))
                .validate(exMatchValidationSettings, parsedSettings == calculatedSettings, s"$parsedSettings vs $calculatedSettings")
          }.result
      }.result
      .toTry
      .flatMap(_ => parsedParamsTry.flatMap(p => parsedValidationSettingsTry.map(vs => (p, vs))))
  }

  def process(header: Header, extensionOpt: Option[Extension]): Try[ErgoStateContext] = {
    def newHeaders: Seq[Header] = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)

    Try {
      val headerVotes: Array[Byte] = header.votes
      val height = header.height
      val votes = headerVotes.filter(_ != Parameters.NoParameter)
      val epochStarts = header.votingStarts(votingEpochLength)
      val forkVote = votes.contains(Parameters.SoftFork)

      val state = ModifierValidator(validationSettings)
        .validateNoThrow(exCheckForkVote, if (forkVote) checkForkVote(height))

      extensionOpt match {
        case Some(extension) if epochStarts =>
          processExtension(extension, header, forkVote)(state).map { processed =>
            val params = processed._1
            val extractedValidationSettings = processed._2
            val proposedVotes = votes.map(_ -> 1)
            val newVoting = VotingData(proposedVotes)
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, params, extractedValidationSettings, newVoting)(ergoSettings)
          }
        case _ =>
          val newVotes = votes
          val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
          state.result.toTry.map { _ =>
            new ErgoStateContext(newHeaders, extensionOpt, genesisStateDigest, currentParameters, validationSettings, newVotingResults)(ergoSettings)
          }
      }
    }.flatten
  }

  /**
    * Checks that header is coming in order.
    *
    * If no last headers collected yet, header is always applicable. It could be genesis header if no pruning, or
    * header at non-genesis height if pruning.
    *
    * @param header - header to be applied to this state context
    * @return Success if header is coming in order, Failure otherwise
    */
  private def checkHeaderHeight(header: Header): Try[Unit] = {
    if (lastHeaders.isEmpty) {
      log.info(s"Last headers are empty, starting assembling state context with $header")
    }
    if (lastHeaders.isEmpty || lastHeaders.head.height + 1 == header.height) {
      Success(())
    } else {
      Failure(new Exception(s"Improper application of $header (last applied header is $lastHeaderOpt: " +
        s"${header.id} => ${lastHeaderOpt.map(_.height + 1)} != ${header.height}"))
    }
  }

  def appendHeader(header: Header, votingSettings: VotingSettings): Try[ErgoStateContext] = {
    validateVotes(header)
      .result
      .toTry
      .flatMap(_ => checkHeaderHeight(header))
      .flatMap { _ =>
        process(header, None)
      }
  }

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param fb             - block to apply
    * @return updated state context or error
    */
  def appendFullBlock(fb: ErgoFullBlock): Try[ErgoStateContext] = Try {
    val votesValidationState = validateVotes(fb.header)
    new ExtensionValidator(votesValidationState, popowAlgos)
      .validateExtension(fb.extension, fb.header, lastExtensionOpt, lastHeaderOpt)
      .validate(bsBlockTransactionsSize,
        fb.blockTransactions.size <= currentParameters.maxBlockSize,
        s"${fb.id} => ${fb.blockTransactions.size} == ${currentParameters.maxBlockSize}")
      .validate(exSize,
        fb.extension.size <= Constants.MaxExtensionSize,
        s"${fb.id} => ${fb.extension.size} == ${Constants.MaxExtensionSize}")
      .result
      .toTry
      .flatMap(_ => checkHeaderHeight(fb.header))
      .flatMap { _ =>
        process(fb.header, Some(fb.extension))
      }
  }.flatten

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"


  /**
    * Check that non-zero votes extracted from block header are correct
    */
  private def validateVotes(header: Header): ValidationState[ErgoStateContext] = {
    val votes: Array[Byte] = header.votes.filter(_ != Parameters.NoParameter)
    val epochStarts = header.votingStarts(votingSettings.votingLength)
    val votesCount = votes.count(_ != Parameters.SoftFork)
    val reverseVotes: Array[Byte] = votes.map(v => (-v).toByte)

    @inline def vs: String = votes.mkString("")

    ModifierValidator(validationSettings)
      .payload(this)
      .validate(hdrVotesNumber, votesCount <= Parameters.ParamVotesCount, s"votesCount=$votesCount")
      .validateSeq(votes) { case (validationState, v) =>
        validationState
          .validate(hdrVotesDuplicates, votes.count(_ == v) == 1, s"Double vote in $vs")
          .validate(hdrVotesContradictory, !reverseVotes.contains(v), s"Contradictory votes in $vs")
          .validate(hdrVotesUnknown, !(epochStarts && !Parameters.parametersDescs.contains(v)), s"Incorrect vote proposed in $vs")
      }
  }

}

object ErgoStateContext {

  def empty(constants: StateConstants): ErgoStateContext = {
    empty(constants.settings.chainSettings.genesisStateDigest, constants.settings)
  }

  def empty(settings: ErgoSettings): ErgoStateContext = {
    empty(settings.chainSettings.genesisStateDigest, settings)
  }

  /**
    * Initialize empty state context
    */
  def empty(genesisStateDigest: ADDigest, settings: ErgoSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, None, genesisStateDigest, LaunchParameters, ErgoValidationSettings.initial,
      VotingData.empty)(settings)
  }

  /**
    * Recovers state context at the beginning of the voting epoch.
    */
  def recover(genesisStateDigest: ADDigest,
              extension: Extension,
              lastHeaders: Seq[Header])
             (settings: ErgoSettings): Try[ErgoStateContext] = {
    val vs = settings.chainSettings.voting
    if (lastHeaders.lastOption.exists(_.height % vs.votingLength == 0)) {
      val currentHeader = lastHeaders.last
      Parameters.parseExtension(currentHeader.height, extension).flatMap { params =>
        ErgoValidationSettings.parseExtension(extension).map { validationSettings =>
          new ErgoStateContext(lastHeaders.reverse, Some(extension), genesisStateDigest, params, validationSettings, VotingData.empty)(settings)
        }
      }
    } else {
      Failure(new Exception("Context could only be recovered at the start of the voting epoch"))
    }
  }

}

case class ErgoStateContextSerializer(ergoSettings: ErgoSettings) extends ScorexSerializer[ErgoStateContext] {

  override def serialize(obj: ErgoStateContext, w: Writer): Unit = {
    /* NOHF PROOF:
    Changed: added assert to not let `UpcomingStateContext` get serialized.
    Motivation: only `ErgoStateContext` is supported in `parse`.
    Safety: `UpcomingStateContext` is used only in `ErgoMiner.createCandidate` and does not get serialized.
  */
    assert(!obj.isInstanceOf[UpcomingStateContext], "UpcomingStateContext serialization is not supported")
    w.putBytes(obj.genesisStateDigest)
    w.putUByte(obj.lastHeaders.size)
    obj.lastHeaders.foreach(h => HeaderSerializer.serialize(h, w))
    VotingDataSerializer.serialize(obj.votingData, w)
    ParametersSerializer.serialize(obj.currentParameters, w)
    ErgoValidationSettingsSerializer.serialize(obj.validationSettings, w)
    w.putUByte(obj.lastExtensionOpt.size)
    obj.lastExtensionOpt.foreach(e => ExtensionSerializer.serialize(e.toExtension(Header.GenesisParentId), w))
  }

  override def parse(r: Reader): ErgoStateContext = {
    val genesisDigest = ADDigest @@ r.getBytes(33)
    val headersLength = r.getUByte()
    val lastHeaders = (1 to headersLength).map(_ => HeaderSerializer.parse(r))
    val votingData = VotingDataSerializer.parse(r)
    val params = ParametersSerializer.parse(r)
    val validationSettings = ErgoValidationSettingsSerializer.parse(r)
    val extensionLength = r.getUByte()
    val lastExtension = (1 to extensionLength).map(_ => ExtensionSerializer.parse(r)).headOption
    new ErgoStateContext(lastHeaders, lastExtension, genesisDigest, params, validationSettings, votingData)(ergoSettings)
  }

}
