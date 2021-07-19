package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import scorex.core.block.Block._
import scorex.util._
import sigmastate.eval.CGroupElement
import sigmastate.eval.Extensions._
import sigmastate.interpreter.CryptoConstants.EcPointType

/**
  * Only header fields that can be predicted by a miner
  */
trait PreHeader {
  val version: Version
  val parentId: ModifierId
  val timestamp: Timestamp
  val nBits: Long
  val height: Int
  val votes: Array[Byte]

  def minerPk: EcPointType

}

case class CPreHeader(version: Version,
                      parentId: ModifierId,
                      timestamp: Timestamp,
                      nBits: Long,
                      height: Int,
                      votes: Array[Byte],
                      minerPk: EcPointType) extends PreHeader

object PreHeader {

  def toSigma(preHeader: PreHeader): special.sigma.PreHeader =
    sigmastate.eval.CPreHeader(
      version = preHeader.version,
      parentId = preHeader.parentId.toBytes.toColl,
      timestamp = preHeader.timestamp,
      nBits = preHeader.nBits,
      height = preHeader.height,
      minerPk = CGroupElement(preHeader.minerPk),
      votes = preHeader.votes.toColl
    )

  def apply(lastHeaderOpt: Option[Header],
            blockVersion: Version,
            pk: EcPointType,
            ts: Long,
            nb: Long,
            v: Array[Byte]): PreHeader = {
    val (pId, h) = AutolykosPowScheme.derivedHeaderFields(lastHeaderOpt)
    CPreHeader(version = blockVersion,
      parentId = pId,
      timestamp = ts,
      nBits = nb,
      height = h,
      votes = v,
      minerPk = pk)
  }

  val fake: PreHeader = CPreHeader(
    version = 0.toByte,
    parentId = Header.GenesisParentId,
    timestamp = 0,
    nBits = Constants.InitialNBits,
    height = ErgoHistory.EmptyHistoryHeight,
    votes = Array.fill(3)(0.toByte),
    minerPk = org.ergoplatform.mining.group.generator
  )

}
