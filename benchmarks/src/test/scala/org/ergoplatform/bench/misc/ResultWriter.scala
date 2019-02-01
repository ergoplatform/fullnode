package org.ergoplatform.bench.misc

import java.io.FileWriter

import scala.io.Source
import scala.util.Try

trait WritableData {
  def toDataLine: String
}

object ResultWriter {

  val linesLimit = 20

  def writeToFile(fileName: String, data: WritableData): Unit = {
    val lines: List[String] = Try(Source.fromFile(fileName).getLines().drop(1).toList).getOrElse(List.empty)
    val resultToSave = if (lines.length < linesLimit) { lines :+ data.toDataLine }
    else { lines.takeRight(linesLimit - 1) :+ data.toDataLine }
    val file = new java.io.File(fileName)
    val writer = new FileWriter(file)
    writer.write("Date,Time\n")
    resultToSave.foreach { l =>
      writer.write(l)
      writer.write('\n')
    }
    writer.flush()
    writer.close()
  }

}

case class Result(t: Long, v: Long) extends WritableData {
  override def toDataLine: String = s"$t,$v"
}
