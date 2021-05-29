package org.ergoplatform.local

import org.ergoplatform.modifiers.history.popow.{NipopowProof, PoPowParams}
import org.ergoplatform.modifiers.history.Header
import scorex.util.ModifierId

/**
  * A verifier for PoPoW proofs. During its lifetime, it processes many proofs with the aim of deducing at any given
  * point what is the best (sub)chain rooted at the specified genesis.
  *
  * @param poPoWParams  - the PoPoW security parameters, m and k
  * @param genesisId    - the block id of the genesis block
  */
class NipopowVerifier(poPoWParams: PoPowParams, genesisId: ModifierId) {
  var bestProof: Option[NipopowProof] = None

  def bestChain: Seq[Header] = {
    bestProof.map(_.headersChain).getOrElse(Seq())
  }

  def process(newProof: NipopowProof) {
    if (newProof.headersChain.head.id == genesisId &&
      !bestProof.exists(_.isBetterThan(newProof))) {
      bestProof = Some(newProof)
    }
  }

}
