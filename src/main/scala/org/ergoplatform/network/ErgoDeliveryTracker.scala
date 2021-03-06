package org.ergoplatform.network


import akka.actor.{ActorRef, ActorSystem}
import scorex.core.network.DeliveryTracker
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._

class ErgoDeliveryTracker(system: ActorSystem,
                          deliveryTimeout: FiniteDuration,
                          maxDeliveryChecks: Int,
                          nvsRef: ActorRef,
                          timeProvider: NetworkTimeProvider)
  extends DeliveryTracker(system, deliveryTimeout, maxDeliveryChecks, nvsRef) {

  def requestedSize: Int = requested.size
}
