package org.ergoplatform.utils

import java.io.{OutputStreamWriter, Writer, ByteArrayOutputStream}

import org.ergoplatform.nodeView.state.UtxoState._
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.metrics._
import scalan.RType
import scalan.util.FileUtil
import scorex.util.ModifierId
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.helpers.SigmaPPrint

import scala.collection.mutable
import scala.util.Success

class MetricsSpec extends ErgoPropertyTest {
  implicit def ergoSettings: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(collectMetrics = true)
  )

  def Coll[T](items: T*)(implicit cT: RType[T]) = CostingSigmaDslBuilder.Colls.fromItems(items:_*)

  class TestCsvCollector extends CsvCollector {
    val outputs = mutable.HashMap.empty[String, ByteArrayOutputStream]

    override protected def createOutputWriter[D](metric: metrics.MetricDesc[D]): Writer = {
      val baos = outputs.getOrElseUpdate(metric.metricName, new ByteArrayOutputStream())
      val w = new OutputStreamWriter(baos)
      writeHeader(metric, w)
      w
    }
  }

  def parseCsvText(text: String): Seq[Seq[String]] = {
    text
      .split(System.lineSeparator).toSeq
      .map(line => line.split(';').dropRight(1): Seq[String])
  }

  def checkOutput[D](c: TestCsvCollector, metric: MetricDesc[D], expOut: Seq[Seq[String]]) = {
    val m1 = c.outputs(metric.metricName)
    val out = parseCsvText(m1.toString())
    if (expOut.isEmpty)
      SigmaPPrint.pprintln(out)
    out shouldBe expOut
  }

  def performMeasuredOps(block: BlockMetricData) = {
    measureOp(block, appendFullBlockMetric) {
      Thread.sleep(5) // to simulate work
      Success(())
    }
    measureCostedOp(block, applyTransactionsMetric) {
      Thread.sleep(10)
      Success(10000L)
    }
    measureOp(block, createUtxoStateMetric) {
      Thread.sleep(15)
      Success(())
    }
  }

  val blockData = BlockMetricData(
    ModifierId @@ "febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d",
    1, Some(2), Parameters.MaxBlockCostDefault)

  val appendFullBlockRows = Seq[Seq[String]](
    Array("blockId", "height", "tx_num", "maxCost", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "1", "2", Parameters.MaxBlockCostDefault.toString, "-1")
  )
  val applyTransactionsRows = Seq[Seq[String]](
    Array("blockId", "height", "tx_num", "maxCost", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "1", "2", Parameters.MaxBlockCostDefault.toString, "10000")
  )
  val createUtxoStateRows = Seq[Seq[String]](
    Array("blockId", "height", "tx_num", "maxCost", "cost"),
    Array("febd2cff9c9bded702d0c73b1f00a3662348d8bf1a59b45c45664faf8498095d", "1", "2", Parameters.MaxBlockCostDefault.toString, "-1")
  )

  property("CsvCollector should output csv rows") {
    val collector = new TestCsvCollector

    metrics.executeWithCollector(collector) {
      performMeasuredOps(blockData)
    }

    collector.flush()

    checkOutput(collector, appendFullBlockMetric, appendFullBlockRows)

    checkOutput(collector, applyTransactionsMetric, applyTransactionsRows)

    checkOutput(collector, createUtxoStateMetric, createUtxoStateRows)
      
    collector.close()
  }

  property("CsvFileCollector should create files") {
    val metricsDir = settings.directory + "/metrics"

    if (FileUtil.file(metricsDir).exists())
      FileUtil.delete(FileUtil.file(metricsDir))

    try {
      val c = new CsvFileCollector(metricsDir)
      metrics.executeWithCollector(c) {
        performMeasuredOps(blockData)
      }
      c.flush()

      val cases = Seq(
        (appendFullBlockMetric, appendFullBlockRows),
        (applyTransactionsMetric, applyTransactionsRows),
        (createUtxoStateMetric, createUtxoStateRows)
      )

      cases.foreach { case (m, expRows) =>
        val f = c.getMetricFile(m)
        f.exists() shouldBe true
        val text = FileUtil.read(f)
        parseCsvText(text) shouldBe expRows
      }

    } finally {
      // cleanup metrics dir
      FileUtil.delete(FileUtil.file(metricsDir))
    }
  }

}
