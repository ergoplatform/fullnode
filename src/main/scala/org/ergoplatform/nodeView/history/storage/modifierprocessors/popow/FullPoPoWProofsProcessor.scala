package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.NipopowProofModifier
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

/**
  * Contains all functions required by History to process PoPoWProofs for regime that accept them.
  */
trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor with HeadersProcessor {

  def validate(m: NipopowProofModifier): Try[Unit] = throw new Error("PoPow not yet supported")

  def process(m: NipopowProofModifier): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}

