package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}
import org.ergoplatform.settings.ChainSettings
import scorex.util.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val chainSettings: ChainSettings) extends ScorexLogging {

  import LinearDifficultyControl._

  val desiredInterval: FiniteDuration = chainSettings.blockInterval
  val useLastEpochs: Int = chainSettings.useLastEpochs
  val epochLength: Int = chainSettings.epochLength
  val initialDifficulty: BigInt = chainSettings.initialDifficulty

  assert(useLastEpochs > 1, "useLastEpochs should always be > 1")
  assert(epochLength > 0, "epochLength should always be > 0")
  assert(epochLength < Int.MaxValue / useLastEpochs, s"epochLength $epochLength is too high for $useLastEpochs epochs")

  /**
    * @return heights of previous headers required for block recalculation
    */
  def previousHeadersRequiredForRecalculation(height: Height): Seq[Int] = {
    if ((height - 1) % epochLength == 0 && height > epochLength * useLastEpochs) {
      (0 to useLastEpochs).map(i => (height - 1) - i * epochLength).reverse
    } else if ((height - 1) % epochLength == 0 && epochLength > 1) {
      (0 to useLastEpochs).map(i => (height - 1) - i * epochLength).filter(_ >= 0).reverse
    } else {
      Seq(height - 1)
    }
  }

  @SuppressWarnings(Array("TraversableHead"))
  def calculate(previousHeaders: Seq[Header]): Difficulty = {
    assert(previousHeaders.nonEmpty, "PreviousHeaders should always contain at least 1 element")
    val uncompressedDiff = {
      if (previousHeaders.lengthCompare(1) == 0 || previousHeaders.head.timestamp >= previousHeaders.last.timestamp) {
        previousHeaders.head.requiredDifficulty
      } else {
        val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
          val start = d.head
          val end = d.last
          require(end.height - start.height == epochLength, s"Incorrect heights interval for $d")
          val diff = end.requiredDifficulty * desiredInterval.toMillis * epochLength / (end.timestamp - start.timestamp)
          (end.height, diff)
        }
        val diff = interpolate(data)
        if (diff >= 1) diff else initialDifficulty
      }
    }
    // perform serialization cycle in order to normalize resulted difficulty
    RequiredDifficulty.decodeCompactBits(
      RequiredDifficulty.encodeCompactBits(uncompressedDiff)
    )
  }

  //y = a + bx
  private[difficulty] def interpolate(data: Seq[(Int, Difficulty)]): Difficulty = {
    val size = data.size
    if (size == 1) {
      data.head._2
    } else {
      val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
      val x: Iterable[BigInt] = data.map(d => BigInt(d._1))
      val x2: Iterable[BigInt] = data.map(d => BigInt(d._1) * d._1)
      val y: Iterable[BigInt] = data.map(d => d._2)
      val xySum = xy.sum
      val x2Sum = x2.sum
      val ySum = y.sum
      val xSum = x.sum

      val b: BigInt = (xySum * size - xSum * ySum) * PrecisionConstant / (x2Sum * size - xSum * xSum)
      val a: BigInt = (ySum * PrecisionConstant - b * xSum) / size / PrecisionConstant

      val point = data.map(_._1).max + epochLength
      a + b * point / PrecisionConstant
    }
  }

}

object LinearDifficultyControl {
  val PrecisionConstant: Int = 1000000000

}
