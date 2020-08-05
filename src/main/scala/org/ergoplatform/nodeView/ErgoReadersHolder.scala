package org.ergoplatform.nodeView

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.nodeView.ErgoReadersHolder._
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.ErgoStateReader
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.util.ScorexLogging

import scala.concurrent.duration._

class ErgoReadersHolder(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
  }

  var historyReaderOpt: Option[ErgoHistoryReader] = None
  var stateReaderOpt: Option[ErgoStateReader] = None
  var mempoolReaderOpt: Option[ErgoMemPoolReader] = None
  var walletReaderOpt: Option[ErgoWalletReader] = None

  @SuppressWarnings(Array("IsInstanceOf"))
  override def receive: Receive = {
    case ChangedHistory(reader: ErgoHistoryReader@unchecked) if reader.isInstanceOf[ErgoHistoryReader] =>
      historyReaderOpt = Some(reader)

    case ChangedState(reader: ErgoStateReader@unchecked) if reader.isInstanceOf[ErgoStateReader] =>
      stateReaderOpt = Some(reader)

    case ChangedMempool(reader: ErgoMemPoolReader@unchecked) if reader.isInstanceOf[ErgoMemPoolReader] =>
      mempoolReaderOpt = Some(reader)

    case ChangedVault(reader: ErgoWalletReader@unchecked) if reader.isInstanceOf[ErgoWalletReader] =>
      walletReaderOpt = Some(reader)

    case GetReaders =>
      (historyReaderOpt, stateReaderOpt, mempoolReaderOpt, walletReaderOpt) match {
        case (Some(h), Some(s), Some(m), Some(w)) => sender ! Readers(h, s, m, w)
        case m =>
          val msgSender = sender()
          context.system.scheduler.scheduleOnce(0.seconds)(self.tell(GetReaders, msgSender))(context.system.dispatcher)
          log.warn(s"Got GetReaders request in state $m")
      }

    case GetDataFromHistory(f) =>
      historyReaderOpt.fold(log.warn("Trying to get data from undefined history reader"))(sender ! f(_))

    case _ =>
    //Do nothing for now. Implement when needed
  }
}

object ErgoReadersHolder {

  case class GetDataFromHistory[A](f: ErgoHistoryReader => A)

  case object GetReaders

  case class Readers(h: ErgoHistoryReader, s: ErgoStateReader, m: ErgoMemPoolReader, w: ErgoWalletReader)

}

object ErgoReadersHolderRef {

  def props(viewHolderRef: ActorRef): Props = Props(new ErgoReadersHolder(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef))

  def apply(viewHolderRef: ActorRef,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef), name)
}
