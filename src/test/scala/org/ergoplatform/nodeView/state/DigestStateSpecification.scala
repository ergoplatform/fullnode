package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core._
import scorex.crypto.authds.ADDigest

import scala.util.Random

class DigestStateSpecification extends ErgoPropertyTest {

  private val emptyVersion: VersionTag = bytesToVersion(Array.fill(32)(0: Byte))
  private val emptyAdDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  property("reopen") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val fb = validFullBlock(parentOpt = None, us, bh)
      val dir2 = createTempDir
      val ds = DigestState.create(Some(us.version), Some(us.rootHash), dir2, stateConstants)
      ds.applyModifier(fb).isSuccess shouldBe true
      ds.close()

      val state = DigestState.create(None, None, dir2, stateConstants)
      state.version shouldEqual fb.header.id
      state.rootHash shouldEqual fb.header.stateRoot
    }
  }

  property("validate() - valid block") {
    var (us, bh) = createUtxoState()
    var ds = createDigestState(us.version, us.rootHash)
    var parentOpt: Option[Header] = None

    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(parentOpt, us, bh, new Random(seed))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block).get
      us = us.applyModifier(block).get
      parentOpt = Some(block.header)
    }
  }

  property("validate() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.validate(b).isFailure shouldBe true
    }
  }

  property("applyModifier() - valid block") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)
      ds.applyModifier(block).isSuccess shouldBe true
    }
  }

  property("applyModifier() - invalid block") {
    forAll(invalidErgoFullBlockGen) { b =>
      val state = createDigestState(emptyVersion, emptyAdDigest)
      state.applyModifier(b).isFailure shouldBe true
    }
  }

  property("rollback & rollback versions") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh)
      bh.sortedBoxes.foreach(box => us.boxById(box.id) should not be None)

      val block = validFullBlock(parentOpt = None, us, bh)

      val ds = createDigestState(us.version, us.rootHash)

      ds.rollbackVersions.size shouldEqual 1

      val ds2 = ds.applyModifier(block).get

      ds2.rollbackVersions.size shouldEqual 2

      ds2.stateContext.lastHeaders.size shouldEqual 1

      java.util.Arrays.equals(ds2.rootHash, ds.rootHash) shouldBe false

      val ds3 = ds2.rollbackTo(ds.version).get
      ds3.rootHash shouldBe ds.rootHash

      ds3.stateContext.lastHeaders.size shouldEqual 0

      ds3.applyModifier(block).get.rootHash shouldBe ds2.rootHash
    }
  }

}
