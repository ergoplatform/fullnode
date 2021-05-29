package org.ergoplatform.utils.generators

import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header, HeaderChain}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, PoPowHeader}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.{BoxUtils, ErgoTestConstants}
import org.ergoplatform.Input
import org.ergoplatform.{ErgoBox, Input}
import scorex.core.block.Block.Version
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._

import scala.util.Random

trait ChainGenerator extends ErgoTestConstants {

  private def emptyProofs = SerializedAdProof @@ scorex.utils.Random.randomBytes(Random.nextInt(5000))

  /** Generates a [[HeaderChain]] of given height starting from a History last block
    */
  def genHeaderChain(height: Int,
                     history: ErgoHistory,
                     diffBitsOpt: Option[Long],
                     useRealTs: Boolean): HeaderChain = {
    val bestHeaderOpt = history.bestHeaderOpt
    val bestHeaderInterlinksOpt = bestHeaderOpt
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))
      .map(ext => NipopowAlgos.unpackInterlinks(ext.fields).get)
      .getOrElse(Seq.empty)
    genHeaderChain(
      height,
      bestHeaderOpt,
      bestHeaderInterlinksOpt,
      history.difficultyCalculator,
      diffBitsOpt = diffBitsOpt,
      useRealTs = useRealTs
    )
  }

  /** Generates a [[HeaderChain]] of given height starting from a given header
    */
  final def genHeaderChain(height: Int,
                           prefixOpt: Option[Header] = None,
                           prefixInterlinksOpt: Seq[ModifierId] = Seq.empty,
                           control: LinearDifficultyControl = defaultDifficultyControl,
                           extensionHash: Digest32 = EmptyDigest32,
                           diffBitsOpt: Option[Long],
                           useRealTs: Boolean): HeaderChain =
    HeaderChain(headerStream(prefixOpt, control, extensionHash, diffBitsOpt, useRealTs).take(height + prefixOpt.size))

  /** Generates a minimal [[HeaderChain]] that satisfies the given condition
    */
  final def genHeaderChain(until: Seq[Header] => Boolean,
                           prefix: Option[Header],
                           control: LinearDifficultyControl,
                           diffBitsOpt: Option[Long],
                           useRealTs: Boolean): HeaderChain = {
    val headers = headerStream(prefix, control, diffBitsOpt = diffBitsOpt, useRealTs = useRealTs)
    val chain = Iterator.from(prefix.size).map(size => headers.take(size)).find(until).get
    HeaderChain(chain)
  }

  def popowHeaderChain(chain: HeaderChain): Seq[PoPowHeader] = {
    chain.headers.foldLeft(Seq.empty[PoPowHeader], None: Option[PoPowHeader]) {
      case ((acc, bestHeaderOpt), h) =>
        val links = popowAlgos.updateInterlinks(
          bestHeaderOpt.map(_.header),
          bestHeaderOpt.map(ph => popowAlgos.interlinksToExtension(ph.interlinks).toExtension(ph.id))
        )
        val poPowH = PoPowHeader(h, links)
        (acc :+ poPowH, Some(poPowH))
    }._1
  }

  private def headerStream(prefix: Option[Header],
                           control: LinearDifficultyControl,
                           extensionHash: Digest32 = EmptyDigest32,
                           diffBitsOpt: Option[Long] = None,
                           useRealTs: Boolean = false): Stream[Header] = {
    val firstHeader = nextHeader(prefix, control, extensionHash, diffBitsOpt = diffBitsOpt, useRealTs = useRealTs)
    lazy val headers: Stream[Header] = firstHeader #:: headers.map(cur =>
      nextHeader(Option(cur), control, extensionHash, diffBitsOpt = diffBitsOpt, useRealTs = useRealTs))
    prefix.toSeq ++: headers
  }

  def nextHeader(prev: Option[Header],
                 control: LinearDifficultyControl,
                 extensionHash: Digest32 = EmptyDigest32,
                 tsOpt: Option[Long] = None,
                 diffBitsOpt: Option[Long] = None,
                 useRealTs: Boolean = false): Header =
    powScheme.prove(
      prev,
      Header.InitialVersion,
      diffBitsOpt.getOrElse(settings.chainSettings.initialNBits),
      EmptyStateRoot,
      EmptyDigest32,
      EmptyDigest32,
      tsOpt.getOrElse(prev.map(_.timestamp + control.desiredInterval.toMillis)
        .getOrElse(if (useRealTs) timeProvider.time() else 0)),
      extensionHash,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get

  def genChain(height: Int): Seq[ErgoFullBlock] =
    blockStream(None).take(height)

  def genChain(height: Int, prefix: ErgoFullBlock): Seq[ErgoFullBlock] =
    blockStream(Option(prefix)).take(height + 1)

  def genChain(height: Int,
               history: ErgoHistory,
               blockVersion: Version = Header.InitialVersion,
               nBits: Long = settings.chainSettings.initialNBits,
               extension: ExtensionCandidate = defaultExtension): Seq[ErgoFullBlock] = {
    val prefix = history.bestFullBlockOpt
    blockStream(prefix, blockVersion, nBits, extension).take(height + prefix.size)
  }

  protected def blockStream(prefix: Option[ErgoFullBlock],
                            blockVersion: Version = Header.InitialVersion,
                            nBits: Long = settings.chainSettings.initialNBits,
                            extension: ExtensionCandidate = defaultExtension): Stream[ErgoFullBlock] = {
    val proof = ProverResult(Array(0x7c.toByte), ContextExtension.empty)
    val inputs = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), proof))
    val minimalAmount = BoxUtils.minimalErgoAmountSimulated(Constants.TrueLeaf, Colls.emptyColl, Map(), parameters)
    val outputs = IndexedSeq(testBox(minimalAmount, Constants.TrueLeaf, creationHeight = startHeight))

    def txs(i: Long) = Seq(ErgoTransaction(inputs, outputs))

    lazy val blocks: Stream[ErgoFullBlock] =
      nextBlock(prefix, txs(1), extension, blockVersion, nBits) #::
        blocks.zip(Stream.from(2)).map { case (prev, i) =>
          nextBlock(Option(prev), txs(i), extension, blockVersion, nBits)
        }
    prefix ++: blocks
  }

  def nextBlock(prev: Option[ErgoFullBlock],
                txs: Seq[ErgoTransaction],
                extension: ExtensionCandidate,
                blockVersion: Version = Header.InitialVersion,
                nBits: Long = settings.chainSettings.initialNBits): ErgoFullBlock = {
    val interlinks = prev.toSeq.flatMap(x =>
      popowAlgos.updateInterlinks(x.header, NipopowAlgos.unpackInterlinks(x.extension.fields).get))
    val validExtension = extension ++ popowAlgos.interlinksToExtension(interlinks)
    powScheme.proveBlock(
      prev.map(_.header),
      blockVersion,
      nBits,
      EmptyStateRoot,
      emptyProofs,
      txs,
      Math.max(timeProvider.time(), prev.map(_.header.timestamp + 1).getOrElse(timeProvider.time())),
      validExtension,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get
  }

  def applyHeaderChain(historyIn: ErgoHistory, chain: HeaderChain): ErgoHistory = {
    var history = historyIn
    chain.headers.foreach { header =>
      history = history.append(header).get._1
    }
    history
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    def appendOrPass(mod: ErgoPersistentModifier, history: ErgoHistory) =
      if (history.contains(mod)) history else history.append(mod).get._1
    blocks.foldLeft(historyIn) { (history, block) =>
      val historyWithBlockHeader = appendOrPass(block.header, history)
      val historyWithTxs = appendOrPass(block.blockTransactions, historyWithBlockHeader)
      val historyWithExtension = appendOrPass(block.extension, historyWithTxs)
      block.adProofs.map(p => appendOrPass(p, historyWithExtension)).getOrElse(historyWithExtension)
    }
  }

  def applyBlock(historyIn: ErgoHistory, block: ErgoFullBlock): ErgoHistory = applyChain(historyIn, Seq(block))

  def applySection(historyIn: ErgoHistory, section: BlockSection): ErgoHistory = historyIn.append(section).get._1

}
