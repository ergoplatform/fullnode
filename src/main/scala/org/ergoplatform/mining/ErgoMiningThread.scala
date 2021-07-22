package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.settings.ErgoSettings
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.util.Random

/** ErgoMiningThread is a scala implementation of a miner using just CPU.
  * It tries to mimic GPU miner's behavior as to polling for new Candidates
  * and submitting solutions. Note that it is useful only for low mining difficulty
  * as its hashrate is just 1000 h/s */
class ErgoMiningThread(
  ergoSettings: ErgoSettings,
  candidateGenerator: ActorRef,
  sk: PrivateKey
) extends Actor
  with ScorexLogging {

  import org.ergoplatform.mining.ErgoMiningThread._

  private val powScheme = ergoSettings.chainSettings.powScheme
  private val NonceStep = 1000

  override def preStart(): Unit = {
    log.info(s"Starting miner thread: ${self.path.name}")
    // poll for new candidate periodically
    context.system.scheduler.scheduleWithFixedDelay(
      1.second,
      ergoSettings.nodeSettings.internalMinerPollingInterval,
      candidateGenerator,
      GenerateCandidate(Seq.empty, reply = true)
    )(context.dispatcher, self)
  }

  override def postStop(): Unit =
    log.info(s"Stopping miner thread: ${self.path.name}")

  override def receive: Receive = {
    case StatusReply.Success(Candidate(candidateBlock, _, _)) =>
      log.info(s"Initiating block mining")
      context.become(mining(nonce = 0, candidateBlock, solvedBlocksCount = 0))
      self ! MineCmd
    case StatusReply.Error(ex) =>
      log.error(s"Preparing candidate did not succeed", ex)
  }

  def mining(
    nonce: Int,
    candidateBlock: CandidateBlock,
    solvedBlocksCount: Int
  ): Receive = {
    case StatusReply.Success(Candidate(cb, _, _)) =>
      // if we get new candidate instead of a cached one, mine it
      if (cb.timestamp != candidateBlock.timestamp) {
        context.become(mining(nonce = 0, cb, solvedBlocksCount))
        self ! MineCmd
      }
    case StatusReply.Error(ex) =>
      log.error(s"Accepting solution or preparing candidate did not succeed", ex)
    case StatusReply.Success(()) =>
      log.info(s"Solution accepted")
      context.become(mining(nonce, candidateBlock, solvedBlocksCount + 1))
    case MineCmd =>
      val lastNonceToCheck = nonce + NonceStep
      powScheme.proveCandidate(candidateBlock, sk, nonce, lastNonceToCheck) match {
        case Some(newBlock) =>
          log.info(s"Found solution, sending it for validation")
          candidateGenerator ! newBlock.header.powSolution
        case None =>
          log.info(s"Trying nonce $lastNonceToCheck")
          context.become(mining(lastNonceToCheck, candidateBlock, solvedBlocksCount))
          self ! MineCmd
      }
    case GetSolvedBlocksCount =>
      sender() ! SolvedBlocksCount(solvedBlocksCount)
  }

}

object ErgoMiningThread {

  case object MineCmd
  case object GetSolvedBlocksCount // metric just for testing purposes for now
  case class SolvedBlocksCount(count: Int)

  private def props(ergoSettings: ErgoSettings, minerRef: ActorRef, sk: BigInt): Props =
    Props(new ErgoMiningThread(ergoSettings, minerRef, sk))

  def apply(ergoSettings: ErgoSettings, minerRef: ActorRef, sk: BigInt)(
    implicit context: ActorRefFactory
  ): ActorRef =
    context.actorOf(props(ergoSettings, minerRef, sk), s"ErgoMiningThread-${Random.alphanumeric.take(5).mkString}")

}
