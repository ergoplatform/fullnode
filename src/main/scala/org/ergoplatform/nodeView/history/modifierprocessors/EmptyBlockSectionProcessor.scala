package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * Trait that implements BlockSectionProcessor interfaces for a regime where the node only
  * downloads block headers
  */
trait EmptyBlockSectionProcessor extends BlockSectionProcessor {

  override protected def process(m: BlockSection): ProgressInfo[ErgoPersistentModifier] =
    ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  override protected def validate(m: BlockSection): Try[Unit] =
    Failure(new Error("Regime that does not support block sections processing"))

}
