package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.settings._
import org.ergoplatform.mining.{AutolykosPowScheme, pkToBytes}
import org.ergoplatform.modifiers.history.PreHeader
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.settings.Constants
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.util.{Failure, Success, Try}

/**
  * State context with predicted header.
  * The predicted header only contains fields that can be predicted.
  */
class UpcomingStateContext(lastHeaders: Seq[Header],
                           predictedHeader: PreHeader,
                           genesisStateDigest: ADDigest,
                           currentParameters: Parameters,
                           votingData: VotingData)(implicit votingSettings: VotingSettings)
  extends ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings) {

  override val lastBlockMinerPk: Array[Byte] = pkToBytes(predictedHeader.minerPk)

  override val previousStateDigest: ADDigest = lastHeaders.lastOption.map(_.stateRoot).getOrElse(genesisStateDigest)

  override val currentHeight: Int = predictedHeader.height

  override def toString: String = s"UpcomingStateContext($predictedHeader, $lastHeaders)"
}

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders        - fixed number of last headers
  * @param genesisStateDigest - genesis state digest (before the very first block)
  * @param currentParameters  - parameters at the beginning of the current voting epoch
  * @param votingData         - votes for parameters change within the current voting epoch
  */
class ErgoStateContext(val lastHeaders: Seq[Header],
                       val genesisStateDigest: ADDigest,
                       val currentParameters: Parameters,
                       val votingData: VotingData)
                      (implicit val votingSettings: VotingSettings)
  extends BytesSerializable with ScorexEncoding {

  val lastBlockMinerPk: Array[Byte] = lastHeaders.headOption
    .map(_.powSolution.encodedPk)
    .getOrElse(Array.fill(32)(0: Byte))

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  val currentHeight: Int = ErgoHistory.heightOf(lastHeaderOpt)

  override type M = ErgoStateContext

  def votingEpochLength: Int = votingSettings.votingLength

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  override def serializer: Serializer[M] = ErgoStateContextSerializer(votingSettings)

  def upcoming(minerPk: EcPointType,
               timestamp: Long,
               nBits: Long,
               votes: Array[Byte],
               version: Byte,
               powScheme: AutolykosPowScheme): ErgoStateContext = {
    val upcomingHeader = PreHeader(lastHeaderOpt, version, minerPk, timestamp, nBits, votes, powScheme)
    val forkVote = votes.contains(Parameters.SoftFork)
    val calculatedParams = currentParameters.update(upcomingHeader.height, forkVote, votingData.epochVotes, votingSettings)
    new UpcomingStateContext(lastHeaders, upcomingHeader, genesisStateDigest, calculatedParams, votingData)
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
    * Extracts parameters from [[Extension]] and compares them to locally calculated once.
    */
  def processExtension(extension: Extension, header: Header, forkVote: Boolean): Try[Parameters] = {
    val height = header.height

    Parameters.parseExtension(height, extension).flatMap { parsedParams =>
      val calculatedParams = currentParameters.update(height, forkVote, votingData.epochVotes, votingSettings)

      if (calculatedParams.blockVersion != header.version) {
        throw new Exception("Versions in header and parameters section are different")
      }

      Try(Parameters.matchParameters(parsedParams, calculatedParams)).map(_ => calculatedParams)
    }
  }

  def process(header: Header, extension: Extension): Try[ErgoStateContext] = process(header, Some(extension))

  def process(header: Header, extensionOpt: Option[Extension]): Try[ErgoStateContext] = Try {

    val headerVotes: Array[Byte] = header.votes
    val height = header.height

    val votes = headerVotes.filter(_ != Parameters.NoParameter)

    val epochStarts = header.votingStarts(votingEpochLength)

    val forkVote = votes.contains(Parameters.SoftFork)

    if (forkVote) checkForkVote(height)

    if (epochStarts) {
      val extractedParams = extensionOpt
        .map(processExtension(_, header, forkVote))
        .getOrElse(Success(currentParameters))

      extractedParams.map { params =>
        val proposedVotes = votes.map(_ -> 1)
        val newVoting = VotingData(proposedVotes)
        new ErgoStateContext(lastHeaders, genesisStateDigest, params, newVoting)(votingSettings)
      }
    } else {
      val newVotes = votes
      val newVotingResults = newVotes.foldLeft(votingData) { case (v, id) => v.update(id) }
      Success(new ErgoStateContext(lastHeaders, genesisStateDigest, currentParameters, newVotingResults)(votingSettings))
    }
  }.flatten


  def appendFullBlock(fullBlock: ErgoFullBlock, votingSettings: VotingSettings): Try[ErgoStateContext] =
    appendBlock(fullBlock.header, Some(fullBlock.extension), votingSettings)

  /**
    * This function verifies whether a full block is valid against the ErgoStateContext instance, and modifies
    * the latter according to the former.
    *
    * @param header      - header of a block
    * @param extensionOpt - extension section of a block (could be missed then only header data being used for update)
    * @param votingSettings - chain-wide voting settings
    * @return updated state context or error
    */
  def appendBlock(header: Header,
                  extensionOpt: Option[Extension],
                  votingSettings: VotingSettings): Try[ErgoStateContext] = Try {
    val height = header.height

    if (height != lastHeaderOpt.map(_.height + 1).getOrElse(ErgoHistory.GenesisHeight)) {
      throw new Exception(s"Improper header applied: $header to $this")
    }

    process(header, extensionOpt).map { sc =>
      val newHeaders = header +: lastHeaders.take(Constants.LastHeadersInContext - 1)
      sc.updateHeaders(newHeaders)
    }
  }.flatten

  def updateHeaders(newHeaders: Seq[Header]): ErgoStateContext = {
    new ErgoStateContext(newHeaders, genesisStateDigest, currentParameters, votingData)(votingSettings)
  }

  override def toString: String =
    s"ErgoStateContext($currentHeight, ${encoder.encode(previousStateDigest)}, $lastHeaders, $currentParameters)"
}

object ErgoStateContext {

  def empty(constants: StateConstants): ErgoStateContext = {
    implicit val votingSettings: VotingSettings = constants.votingSettings
    new ErgoStateContext(Seq.empty, constants.genesisStateDigest, LaunchParameters, VotingData.empty)
  }

  def empty(genesisStateDigest: ADDigest, votingSettings: VotingSettings): ErgoStateContext = {
    new ErgoStateContext(Seq.empty, genesisStateDigest, LaunchParameters, VotingData.empty)(votingSettings)
  }

  /**
    * Recovers state context at the beginning of the voting epoch.
    */
  def recover(genesisStateDigest: ADDigest,
              extension: Extension,
              lastHeaders: Seq[Header])
             (vs: VotingSettings): Try[ErgoStateContext] = {
    if (lastHeaders.lastOption.exists(_.height % vs.votingLength == 0)) {
      val currentHeader = lastHeaders.last
      Parameters.parseExtension(currentHeader.height, extension).map { params =>
        new ErgoStateContext(lastHeaders.reverse, genesisStateDigest, params, VotingData.empty)(vs)
      }
    } else {
      Failure(new Exception("Context could only be recovered at the start of the voting epoch"))
    }
  }

}

case class ErgoStateContextSerializer(votingSettings: VotingSettings) extends Serializer[ErgoStateContext] {

  override def toBytes(ctx: ErgoStateContext): Array[Byte] = {
    val lastHeaderBytes = scorex.core.utils.concatBytes(ctx.lastHeaders.map(_.bytes))

    val votingDataBytes = VotingDataSerializer.toBytes(ctx.votingData)
    val votingDataSize = Ints.toByteArray(votingDataBytes.length)

    Bytes.concat(
      ctx.genesisStateDigest,
      Ints.toByteArray(lastHeaderBytes.length),
      lastHeaderBytes,
      votingDataSize,
      votingDataBytes,
      ParametersSerializer.toBytes(ctx.currentParameters))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val lastHeaderBytesLength = Ints.fromByteArray(bytes.slice(33, 37))

    def loop(bytes: Array[Byte], offset: Int, acc: Seq[Header]): Seq[Header] =
      if (offset < lastHeaderBytesLength) {
        val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
        loop(bytes, offset + header.bytes.length, header +: acc)
      } else {
        acc.reverse
      }

    val afterHeaders = 37 + lastHeaderBytesLength
    val lastHeaderBytes = bytes.slice(37, afterHeaders)
    val lastHeaders = loop(lastHeaderBytes, 0, Seq.empty)

    val votingDataSize = Ints.fromByteArray(bytes.slice(afterHeaders, afterHeaders + 4))

    val afterVoting = afterHeaders + 4 + votingDataSize
    VotingDataSerializer.parseBytes(bytes.slice(afterHeaders + 4, afterVoting)).flatMap { votingData =>
      ParametersSerializer.parseBytes(bytes.slice(afterVoting, bytes.length)).map { params =>
        new ErgoStateContext(lastHeaders, genesisDigest, params, votingData)(votingSettings)
      }
    }
  }.flatten

}
