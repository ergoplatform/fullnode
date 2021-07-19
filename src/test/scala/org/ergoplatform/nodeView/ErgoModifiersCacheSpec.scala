package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoPropertyTest, HistoryTestHelpers}
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, bytesToId}

import scala.annotation.tailrec

class ErgoModifiersCacheSpec extends ErgoPropertyTest with HistoryTestHelpers {

  private def genKey(i: Int): ModifierId = bytesToId(Blake2b256(s"$i"))

  private def genCachePair(i: Int): (ModifierId, Header) = {
    val header = defaultHeaderGen.sample.value
    val k = genKey(i)
    k -> header
  }

  property("cache size is within limits") {
    val limit = 3
    val modifiersCache = new ErgoModifiersCache(limit)

    modifiersCache.maxSize shouldBe limit

    (1 to limit).foreach { i =>
      val (k, h) = genCachePair(i)
      modifiersCache.put(k, h)
    }

    modifiersCache.size shouldBe limit

    val above = genCachePair(limit + 1)

    modifiersCache.put(above._1, above._2)
    modifiersCache.size shouldBe (limit + 1)

    modifiersCache.cleanOverfull()

    modifiersCache.size shouldBe limit

    modifiersCache.remove(genKey(1)).isEmpty shouldBe true
  }

  property("cache is proposing a reasonable candidate to enhance history") {
    val limit = 25
    val modifiersCache = new ErgoModifiersCache(limit)

    val history0 = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

    val chain = genChain(5, history0)

    chain.foreach { fb =>
      modifiersCache.put(fb.header.id, fb.header)
      modifiersCache.put(fb.header.transactionsId, fb.blockTransactions)
      modifiersCache.put(fb.header.ADProofsId, fb.adProofs.value)
    }

    //The history is empty - we can apply only a header at height == 0 at this moment.
    //Out of 15 elements in the cache, the cache should propose a proper candidate
    val c1 = modifiersCache.popCandidate(history0).value
    c1.isInstanceOf[Header] shouldBe true
    val h1 = c1.asInstanceOf[Header]
    h1.height shouldBe ErgoHistory.GenesisHeight

    val history1 = history0.append(c1).get._1

    //We have only header of height == 0 in the history, so cache should return whether a header of height == 1
    //or a non-header part of the full block at height == 0
    val c2 = modifiersCache.popCandidate(history1).value
    val properCandidate = c2 match {
      case h: Header => h.height == 1
      case bt: BlockTransactions => bt.id == h1.transactionsId
      case ap: ADProofs => ap.id == h1.ADProofsId
    }
    properCandidate shouldBe true
  }

  property("cache is proposing proper candidate during forking") {
    val limit = 25
    val modifiersCache = new ErgoModifiersCache(limit)

    var history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

    val chain = genChain(1, history)

    chain.foreach{fb => history = applyBlock(history, fb)}

    val chain1 = genChain(5, history).tail

    val chain2 = genChain(10, history).tail

    chain1.foreach(fb => history = applyBlock(history, fb))

    chain2.foreach(fb => history = history.append(fb.header).get._1)

    history.bestFullBlockOpt.value shouldBe chain1.last
    history.bestHeaderOpt.value shouldBe chain2.last.header

    chain2.flatMap(_.blockSections).foreach(s => modifiersCache.put(s.id, s))

    @tailrec
    def applyLoop(): Unit = {
      modifiersCache.popCandidate(history) match {
        case Some(mod) =>
          history.append(mod)
          applyLoop()
        case None =>
          modifiersCache.size shouldBe 0
          history.bestFullBlockOpt.value shouldBe chain2.last
      }
    }
    applyLoop()
  }

}
