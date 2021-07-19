package org.ergoplatform.tools

import org.ergoplatform.mining.difficulty.{LinearDifficultyControl, RequiredDifficulty}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.generators.ErgoGenerators

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

/**
  * Object that allows to simulate blockchain with the specified difficulty control function and estimate deviation
  * of block interval from the desired one
  */
object DifficultyControlSimulator extends App with ErgoGenerators {

  val baseHeader = defaultHeaderGen.sample.get
//  val difficultyControl = new LinearDifficultyControl(1.minute, useLastEpochs = 100, epochLength = 1)
  val chainSettings = settings.chainSettings.copy(blockInterval = 2.minute, useLastEpochs = 8, epochLength = 256)
  val difficultyControl = new LinearDifficultyControl(chainSettings)
  // Constant rate: Stable simulated average interval = 119713, error  = 0.23916666% | Init simulated average interval = 117794, error  = 1.8383334%
  // Increasing rate: Stable simulated average interval = 119841, error  = 0.1325% | Init simulated average interval = 119077, error  = 0.76916665%
  // Random rate: Stable simulated average interval = 120539, error  = 0.44916666% | Init simulated average interval = 115519, error  = 3.7341666%

  blockchainSimulator(difficultyControl,
    baseHeader.copy(height = 0, timestamp = 0, nBits = 16842752),
    randomHashRate)

  /**
    * Generate blockchain starting from initial header with specified difficulty control and measure mean time interval between blocks
    *
    * @param difficultyControl
    * @param initialHeader
    */
  def blockchainSimulator(difficultyControl: LinearDifficultyControl,
                          initialHeader: Header,
                          timeForOneHash: Int => Int): Unit = {
    // number of blocks in simulated chain
    val chainLength = 100000

    val curChain = mutable.Map[Int, Header](initialHeader.height -> initialHeader)

    @tailrec
    def genchain(curHeight: Int): mutable.Map[Int, Header] = {

      if (curHeight >= chainLength) {
        curChain
      } else {
        val lastHeader = curChain(curHeight)
        val requiredDifficulty = requiredDifficultyAfter(lastHeader, curChain)
        val target = 1.toDouble / requiredDifficulty.toDouble

        val hashTime = timeForOneHash(curHeight)

        @tailrec
        def simulateTimeDiff(currentDiff: Long = 0): Long = {
          val hit = Random.nextDouble()
          if (hit < target) {
            currentDiff + hashTime
          } else {
            simulateTimeDiff(currentDiff + hashTime)
          }
        }

        val timeDiff = simulateTimeDiff()
        val newHeader = lastHeader.copy(timestamp = lastHeader.timestamp + timeDiff,
          height = curHeight + 1,
          nBits = RequiredDifficulty.encodeCompactBits(requiredDifficulty))
        curChain(newHeader.height) = newHeader

        genchain(curHeight + 1)
      }
    }

    val chain = genchain(initialHeader.height)

    printEpochs(chain.values.toSeq.sortBy(_.height), difficultyControl)


  }

  def printTestnetData(): Unit = {
    val baseHeader = defaultHeaderGen.sample.get
    val chainSettings = ErgoSettings.read().chainSettings.copy(epochLength = 1)
    val difficultyControl = new LinearDifficultyControl(chainSettings)

    val headers = Source.fromResource("difficulty.csv").getLines().toSeq.tail.map { line =>
      val l = line.split(",")
      baseHeader.copy(
        height = l(0).toInt,
        timestamp = l(1).toLong,
        nBits = RequiredDifficulty.encodeCompactBits(BigInt(l(2)))
      )
    }
    printEpochs(headers, difficultyControl)
  }

  def printEpochs(headers: Seq[Header], difficultyControl: LinearDifficultyControl): Unit = {
    case class Epoch(startHeight: Int, requiredDifficulty: BigInt, blockInterval: FiniteDuration, timestamp: Long) {
      val realDifficulty: BigInt = requiredDifficulty * difficultyControl.desiredInterval.toMillis / blockInterval.toMillis

      override def toString: String = s"$startHeight,$requiredDifficulty,$realDifficulty,${blockInterval.toMillis}"
    }

    val epochs: Seq[Epoch] = headers.filter(h => h.height % difficultyControl.epochLength == 0).sliding(2).map { p =>
      val start = p.head
      val end = p.last
      val meanInterval = ((end.timestamp - start.timestamp) / (end.height - start.height)).millis

      Epoch(start.height, end.requiredDifficulty, meanInterval, end.timestamp)
    }.toSeq

    val initEpochs: Seq[Epoch] = epochs.take(difficultyControl.useLastEpochs).tail
    val stableEpochs: Seq[Epoch] = epochs.drop(difficultyControl.useLastEpochs)
    val simulatedInit = initEpochs.map(_.blockInterval.toMillis).sum / initEpochs.length
    val simulatedStable = stableEpochs.map(_.blockInterval.toMillis).sum / stableEpochs.length
    val desired = difficultyControl.desiredInterval.toMillis
    val errorInit = Math.abs(simulatedInit - desired).toFloat * 100 / desired
    val errorStable = Math.abs(simulatedStable - desired).toFloat * 100 / desired

    println(s"Control:")
    println(s"Desired interval = $desired, epoch length = ${difficultyControl.epochLength}, use last epochs = " +
      difficultyControl.useLastEpochs)
    println(s"Init simulated average interval = $simulatedInit, error  = $errorInit%")
    println(s"Stable simulated average interval = $simulatedStable, error  = $errorStable%")

/*
    println(s"height,requiredDifficulty,realDifficulty,timeDiff")
    epochs.foreach(d => println(d))
*/
  }


  private def requiredDifficultyAfter(parent: Header, blockchain: mutable.Map[Int, Header]): Difficulty = {
    val parentHeight = parent.height
    val heights = difficultyControl.previousHeadersRequiredForRecalculation(parentHeight + 1)
      .ensuring(_.last == parentHeight)
    if (heights.lengthCompare(1) == 0) {
      difficultyControl.calculate(Seq(parent))
    } else {
      val headersToCalculate = heights.map(h => blockchain(h))
      difficultyControl.calculate(headersToCalculate)
    }
  }


  def constantHashRate(height: Int): Int = 1000
  def linearGrowingHashRate(height: Int): Int = Math.max(2000 - height / 20, 100)
  def randomHashRate(height: Int): Int = Random.nextInt(1000)

}
