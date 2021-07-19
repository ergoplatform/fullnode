package org.ergoplatform.nodeView

import org.ergoplatform.Utils
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.BlockTransactions

trait NVBenchmark {

  val resourceUrlPrefix = "https://github.com/ergoplatform/static-data/raw/master/v2"

  def readHeaders: Seq[Header] = readModifiers[Header](s"$resourceUrlPrefix/headers.dat")
  def readPayloads: Seq[BlockTransactions] = readModifiers[BlockTransactions](s"$resourceUrlPrefix/payloads.dat")
  def readExtensions: Seq[Extension] = readModifiers[Extension](s"$resourceUrlPrefix/extensions.dat")
  def readBlocks: Seq[ErgoFullBlock] = readHeaders.zip(readPayloads).zip(readExtensions)
    .map { case ((h, txs), ext) => ErgoFullBlock(h, txs, ext, None) }

  def readModifiers[M <: ErgoPersistentModifier](path: String): Seq[M] = {
    val is = Utils.getUrlInputStream(path)
    Stream
      .continually {
        Utils.readModifier[M](is)
      }
      .takeWhile(_.isDefined)
      .flatten
      .toList
  }

}
