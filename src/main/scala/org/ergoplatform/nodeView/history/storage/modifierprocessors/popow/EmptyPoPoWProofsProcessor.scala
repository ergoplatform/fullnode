package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.NipopowProofModifier
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that do not accept them.
  */
trait EmptyPoPoWProofsProcessor extends PoPoWProofsProcessor {

  def validate(m: NipopowProofModifier): Try[Unit] = Failure(new Error("Regime that do not process PoPoWProof"))

  def process(m: NipopowProofModifier): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}

