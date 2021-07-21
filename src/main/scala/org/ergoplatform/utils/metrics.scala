package org.ergoplatform.utils

import java.io.{BufferedWriter, FileWriter, File, Writer}

import org.ergoplatform.settings.ErgoSettings
import scalan.util.FileUtil

import scala.util.{DynamicVariable, Try}
import scorex.core.validation.ValidationResult
import scorex.util.ModifierId

import scala.collection.mutable

/** Implementation of the metrics sub-system. The general algorithm of metrics collection is as the
  * following:
  * 1. The `block` of code is wrapped into invocation of [[metrics.measureOp]] or
  * [[metrics.measureCostedOp]] method.
  * 2. The method executes the `block` and if `ergo.node.collectMetrics` config parameter is `true`
  * then execution time is measured.
  * 3. If metric collection is enabled then [[metrics.MeasuredData]] instance is created and passed
  * to the current collector on the thread [[metrics.MetricsCollector.current]] by calling
  * [[metrics.MetricsCollector.collect]] method.
  * 4. The [[metrics.CsvCollector]] assembles a new csv line, looks up the
  * [[metrics.CsvCollector.OutputManager]], and writes the line into the output Writer.
  *
  * The current collector on the current thread can be changed by using the following pattern
  * <pre class="stHighlight">
  * metrics.executeWithCollector(MyCollector) {
  *   // execute code with the given collector set as current
  * }
  * </pre>
  */
object metrics {

  /** Type-class which implements capability to output data as CSV-like row. */
  trait MetricRow[D] {
    /** Returns an array of columns in a CSV-like table */
    def fieldNames: Seq[String]
    /** Returns a CSV-like row for the given data instance.
      * @param d an instance to transform into a row of strings
      * @return an array of cells in a CSV-like row
      */
    def fieldValues(d: D): Seq[String]
  }

  /** Represents a descriptor for the given metric name and data type.
    * Conceptually a descriptor specifies is a table of rows with the set of columns given by `row`
    * instance.
    * @tparam D data type which represents information recorded for each measurement of this metric
    * @param metricName a name of a metric
    * @param row an instance of CSV-like row handler
    */
  case class MetricDesc[D](metricName: String)(implicit row: MetricRow[D]) {
    /** If metric is a table, then this method returns a the names of the columns. */
    def fieldNames: Seq[String] = row.fieldNames
    /** Extract the values of the row from the given data instance. */
    def fieldValues(d: D): Seq[String] = row.fieldValues(d)
  }

  /** Metric data, which describes a block-related operations.
    *
    * @param blockId          identifier of the related block
    * @param height           blockchain height of the block
    * @param nTransactionsOpt optional number of transactions in the block
    * @param maxBlockCost     limit on the maximal cost of block validation
    */
  case class BlockMetricData(
    blockId: ModifierId,
    height: Int,
    nTransactionsOpt: Option[Int],
    maxBlockCost: Long)

  implicit object BlockMetricRow extends MetricRow[BlockMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "height", "tx_num", "maxCost")
    override def fieldValues(d: BlockMetricData): Seq[String] = {
      val nTx = d.nTransactionsOpt.map(_.toString).getOrElse("")
      Array(d.blockId, d.height.toString, nTx, d.maxBlockCost.toString)
    }
  }

  /** Metric data, which describes a transaction-related operations.
    *
    * @param blockId identifier of the related block
    * @param txId    identifier of the related transaction
    */
  case class TransactionMetricData(blockId: Option[ModifierId], txId: ModifierId)

  implicit object TransactionMetricRow extends MetricRow[TransactionMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId")
    override def fieldValues(d: TransactionMetricData): Seq[String] = {
      Array(d.blockId.getOrElse(""), d.txId)
    }
  }

  /** Metric data for an operation related to transaction input with the given `index`.
    *
    * @param blockId block identifier of the owner block
    * @param txId    transaction identifier
    * @param index   zero-based index of the input in the transaction
    */
  case class InputMetricData(blockId: Option[ModifierId], txId: ModifierId, index: Int)

  implicit object InputMetricRow extends MetricRow[InputMetricData] {
    override val fieldNames: Seq[String] = Array("blockId", "txId", "index")
    override def fieldValues(d: InputMetricData): Seq[String] =
      Array(d.blockId.getOrElse(""), d.txId, d.index.toString)
  }

  /** Data obtained by measured operation. Consists of the metric data, the cost computed by the
    * operation and measured time.
    *
    * @tparam D type of metric data
    * @param data     and instance of metric data
    * @param cost     computed cost of operation
    * @param timeNano measured operation time in nanoseconds
    */
  case class MeasuredData[D](data: D, cost: Long, timeNano: Long)

  /** Base class of metric collectors. */
  abstract class MetricsCollector {
    /** Collects the given data, of the given metric. */
    def collect[D](data: MeasuredData[D], metric: MetricDesc[D]): Unit
  }

  object MetricsCollector {
    /** Number of data rows the writer is flushed. */
    val FlushInterval = 50

    private[ergoplatform] val _current = new DynamicVariable[MetricsCollector](null)

    /** Returns the current collector from the current thread. */
    def current: MetricsCollector = {
      val c = _current.value
      if (c == null) DiscardingCollector
      else c
    }

    private object DiscardingCollector extends MetricsCollector {
      override def collect[D](obj: MeasuredData[D], metric: MetricDesc[D]): Unit = () // do nothing
    }
  }

  /** Base class for CSV file collectors. Each collector maintains a dictionary of metrics
    * which may receive data.
    * This class is thread safe. */
  abstract class CsvCollector extends MetricsCollector {

    /** Keeps track of metric descriptor, associated writer and the current number of lines written. */
    class OutputManager(val metric: MetricDesc[_], val writer: Writer, var lineCount: Int = 0)

    /** Keeps one OutputManager for each metric name. */
    val metricOutputs = mutable.HashMap.empty[String, OutputManager]

    /** Creates a suitable Writer for the given metric. */
    protected def createOutputWriter[D](metric: MetricDesc[D]): Writer

    /** Creates a suitable OutputManager for the given metric. */
    protected def createOutputManager[D](metric: MetricDesc[D]): OutputManager = {
      new OutputManager(metric, createOutputWriter(metric))
    }

    /** Write header and thus prepare writer for accepting data rows */
    protected def writeHeader[D](metric: MetricDesc[D], w: Writer): Unit = {
      val fields = metric.fieldNames ++ Array("cost", "time")
      val line = fields.mkString(";") + System.lineSeparator()
      w.write(line)
    }

    override def collect[D](obj: MeasuredData[D], metric: MetricDesc[D]): Unit = this.synchronized {
      val m = metricOutputs.getOrElseUpdate(metric.metricName, createOutputManager(metric))

      val values = metric.fieldValues(obj.data) ++ Array(obj.cost.toString, obj.timeNano.toString)
      val line = values.mkString(";") + System.lineSeparator()
      m.writer.write(line)
      m.lineCount += 1
      if (m.lineCount % MetricsCollector.FlushInterval == 0)
        m.writer.flush()
    }

    /** Flush all the underlying file writers. */
    def flush(): Unit = synchronized {
      metricOutputs.values.foreach { m =>
        m.writer.flush()
      }
    }

    /** Close all the underlying file writers. */
    def close(): Unit = synchronized {
      metricOutputs.values.foreach { m =>
        m.writer.close()
      }
    }
  }

  /** Concrete implementation of CsvCollector saving all data rows into CSV files in the given
    * directory
    */
  class CsvFileCollector(val directory: String) extends CsvCollector {

    /** Returns a File for the given metric. */
    def getMetricFile[D](metric: MetricDesc[D]): File = {
      val fileName = s"${metric.metricName}.csv"
      val dir = FileUtil.file(directory)
      val file = FileUtil.file(dir, fileName)
      file
    }

    /** Creates a new FileWriter wrapped in a BufferedWriter */
    override protected def createOutputWriter[D](metric: MetricDesc[D]): Writer = {
      val file = getMetricFile(metric)
      if(file.exists())
        new BufferedWriter(new FileWriter(file, true))
      else {
        file.getParentFile.mkdirs() // ensure dirs

        // open new empty file
        val w = new BufferedWriter(new FileWriter(file, false))

        writeHeader(metric, w)
        w
      }
    }
  }

  /** Executes the given code block under the given collector.
    * Within the block the collector can be obtaned as MetricsCollector.current.
    * The method saves the current collector before the call and restores it after the block is
    * executed.
    */
  def executeWithCollector[R](collector: MetricsCollector)(block: => R): R = {
    MetricsCollector._current.withValue(collector)(block)
  }

  /** Execute block and measure the time of its execution using Java Virtual Machine's
    * high-resolution time source, in nanoseconds. */
  def measureTimeNano[T](action: => T): (T, Long) = {
    val t0 = System.nanoTime()
    val res = action
    val t = System.nanoTime()
    (res, t - t0)
  }

  /** Helper method which saves the given data of the given metric. */
  @inline def collectMetricsTo[D](metric: MetricDesc[D], data: D, cost: Long, time: Long) = {
    val obj = MeasuredData(data, cost, time)
    val metricCollector = MetricsCollector.current
    metricCollector.collect(obj, metric)
  }

  /** Executes the given `block` of code (operation) measuring its time.
    * Sends the given metric data to the current collector. */
  def measureOp[D, R](data: => D, metric: MetricDesc[D])
                     (block: => Try[R])
                     (implicit es: ErgoSettings): Try[R] = {
    if (es.nodeSettings.collectMetrics) {
      val (res, time) = measureTimeNano(block)
      collectMetricsTo(metric, data, -1, time)
      res
    } else
      block  // no overhead when the metrics are not being collected
  }

  /** Executes the given `costedOp` measuring its time.
    * Sends the given metric data to the current collector.
    * In addition, the cost returned by the operation is stored in the row.
    */
  def measureCostedOp[D, R](data: => D, metric: MetricDesc[D])
                           (costedOp: => Try[Long])
                           (implicit es: ErgoSettings): Try[Long] = {
    if (es.nodeSettings.collectMetrics) {
      val (costTry, time) = measureTimeNano(costedOp)
      val cost = costTry.getOrElse(-1L)
      collectMetricsTo(metric, data, cost, time)
      costTry
    } else
      costedOp // no overhead when the metrics are not being collected
  }

  /** Executes the given `validationOp` measuring its time.
    * Sends the given metric data to the current collector.
    * In addition, the cost returned by the operation is stored in the row.
    */
  def measureValidationOp[D, R](data: => D, metric: MetricDesc[D])
                               (validationOp: => ValidationResult[Long])
                               (implicit es: ErgoSettings): ValidationResult[Long] = {
    if (es.nodeSettings.collectMetrics) {
      val (res, time) = measureTimeNano(validationOp)
      val cost = res.toTry.getOrElse(-1L)
      collectMetricsTo(metric, data, cost, time)
      res
    } else
      validationOp // no overhead when the metrics are not being collected
  }
}
