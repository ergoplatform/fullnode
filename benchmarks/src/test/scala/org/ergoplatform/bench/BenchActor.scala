package org.ergoplatform.bench

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.bench.misc.{Result, ResultWriter}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.StateType
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class BenchActor(threshold: Int, state: StateType) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  var counter = 0
  var start = 0L
  var finish = 0L

  val timeout: FiniteDuration = 2 hours

  val fileName = s"target/bench/result_${state.stateTypeName}"

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    context.system.scheduler.scheduleOnce(timeout, self, BenchActor.Timeout)
  }

  override def receive: Receive = {
    case BenchActor.Start =>
      start = System.currentTimeMillis()
      log.info(s"Starting bench..")
    case SemanticallySuccessfulModifier(_: ErgoFullBlock) =>
      self ! BenchActor.Inc
    case BenchActor.Inc =>
      counter += 1
      if (counter % 100 == 0 ) log.info(s"counter is $counter")
      if (counter >= threshold) {
        finish = System.currentTimeMillis()
        val seconds = (finish - start) / 1000
        log.info(s"Start timestamp is $start")
        log.info(s"Finish timestamp is $finish")
        log.info(s"FINISHED APPLYING $threshold MODIFIERS in $seconds seconds.")
        ResultWriter.writeToFile(s"$fileName$threshold.csv", Result(finish, seconds))
        System.exit(0)
      }
    case BenchActor.Timeout =>
      log.error("Bench is taking too long. Shutting down on timeout.")
      System.exit(1)
  }

}

object BenchActor {

  def apply(threshold: Int, state: StateType)(implicit ac: ActorSystem): ActorRef = {
    ac.actorOf(Props.apply(classOf[BenchActor], threshold, state))
  }

  case object Timeout

  case object Inc

  case object Start
}
