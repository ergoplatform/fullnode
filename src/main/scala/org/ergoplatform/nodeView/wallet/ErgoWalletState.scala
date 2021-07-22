package org.ergoplatform.nodeView.wallet

import com.google.common.hash.BloomFilter
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletState.FilterFn
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.settings.{ErgoSettings, LaunchParameters, Parameters}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import scorex.util.ScorexLogging

case class ErgoWalletState(
    storage: WalletStorage,
    secretStorageOpt: Option[JsonSecretStorage],
    registry: WalletRegistry,
    offChainRegistry: OffChainRegistry,
    outputsFilter: Option[BloomFilter[Array[Byte]]], // Bloom filter for boxes not being spent to the moment
    walletVars: WalletVars,
    stateReaderOpt: Option[ErgoStateReader], //todo: temporary 3.2.x collection and readers
    mempoolReaderOpt: Option[ErgoMemPoolReader],
    utxoStateReaderOpt: Option[UtxoStateReader],
    parameters: Parameters
  ) extends ScorexLogging {

  /**
    * This filter is selecting boxes which are onchain and not spent offchain yet or created offchain
    * (and not spent offchain, but that is ensured by offChainRegistry).
    * This filter is used when the wallet is going through its boxes to assemble a transaction.
    */
  val walletFilter: FilterFn = (trackedBox: TrackedBox) => {
    val preStatus = if (trackedBox.chainStatus.onChain) {
      offChainRegistry.onChainBalances.exists(_.id == trackedBox.boxId)
    } else {
      true
    }

    val bid = trackedBox.box.id

    // double-check that box is not spent yet by inputs of mempool transactions
    def notInInputs: Boolean = {
      mempoolReaderOpt match {
        case Some(mr) => !mr.spentInputs.exists(_.sameElements(bid))
        case None => true
      }
    }

    // double-check that box is exists in UTXO set or outputs of offchain transaction
    def inOutputs: Boolean = {
      utxoStateReaderOpt.forall { utxo =>
        utxo.boxById(bid).isDefined
      }
    }

    preStatus && notInInputs && inOutputs
  }

  // Secret is set in form of keystore file of testMnemonic in the config
  def secretIsSet(testMnemonic: Option[String]): Boolean = secretStorageOpt.nonEmpty || testMnemonic.nonEmpty

  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them).
  // The state context is being updated by listening to state updates.
  def stateContext: ErgoStateContext = storage.readStateContext

  /**
    * @return height of the last block scanned by the wallet
    */
  def getWalletHeight: Int = registry.fetchDigest().height

  /**
    * @return Height of the chain as reported by the state (i.e. height of a last block applied to the state, not the wallet). Wallet's height may be behind it.
    */
  def fullHeight: Int = stateContext.currentHeight

  def getChangeAddress(implicit addrEncoder: ErgoAddressEncoder): Option[P2PKAddress] = walletVars.proverOpt.map { prover =>
    storage.readChangeAddress
      .getOrElse {
        log.debug("Change address not specified. Using root address from wallet.")
        P2PKAddress(prover.hdPubKeys.head.key)
      }
  }

  // Read a box from UTXO set if the node has it, otherwise, from the wallet
  def readBoxFromUtxoWithWalletFallback(boxId: BoxId): Option[ErgoBox] = {
    utxoStateReaderOpt match {
      case Some(utxoReader) =>
        utxoReader.boxById(boxId)
      case None =>
        registry.getBox(boxId).map(_.box)
    }
  }

  // expected height of a next block when the wallet is receiving a new block with the height blockHeight
  def expectedNextBlockHeight(blockHeight: Height, isFullBlocksPruned: Boolean): Height = {
    val walletHeight = getWalletHeight
    if (!isFullBlocksPruned) {
      // Node has all the full blocks and applies them sequentially
      walletHeight + 1
    } else {
      // Node has pruned blockchain
      if (walletHeight == 0) {
        blockHeight // todo: should be height of first non-pruned block
      } else {
        walletHeight + 1
      }
    }
  }

  /**
    * A helper method that returns unspent boxes
    */
  def getBoxesToSpend: Seq[TrackedBox] = {
    require(walletVars.publicKeyAddresses.nonEmpty, "No public keys in the prover to extract change address from")
    (registry.walletUnspentBoxes() ++ offChainRegistry.offChainBoxes).distinct
  }

}

object ErgoWalletState {

  private type FilterFn = TrackedBox => Boolean

  /**
    * This filter is not filtering out anything, used when the wallet works with externally provided boxes.
    */
  val noWalletFilter: FilterFn = (_: TrackedBox) => true

  def initial(ergoSettings: ErgoSettings): ErgoWalletState = {
    val ergoStorage: WalletStorage = WalletStorage.readOrCreate(ergoSettings)(ergoSettings.addressEncoder)
    val registry = WalletRegistry.apply(ergoSettings)
    val offChainRegistry = OffChainRegistry.init(registry)
    val walletVars = WalletVars.apply(ergoStorage, ergoSettings)
    ErgoWalletState(
      ergoStorage,
      secretStorageOpt = None,
      registry,
      offChainRegistry,
      outputsFilter = None,
      walletVars,
      stateReaderOpt = None,
      mempoolReaderOpt = None,
      utxoStateReaderOpt = None,
      LaunchParameters
    )
  }
}
