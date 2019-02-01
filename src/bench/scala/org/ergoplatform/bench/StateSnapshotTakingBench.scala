package org.ergoplatform.bench

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import io.iohk.iodb.LSMStore
import org.ergoplatform.ErgoBox
import org.ergoplatform.bench.misc.TempDir
import org.ergoplatform.bench.misc.Utils.elapsedTimeOf
import org.ergoplatform.nodeView.state.ErgoState.genesisEmissionBox
import org.ergoplatform.nodeView.state.UtxoState.createPersistentProver
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import scorex.util.encode.Base16
import sigmastate.Values

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object StateSnapshotTakingBench extends ScorexLogging {

  // Current result for state size = 500000 is 24 millis before rollback
  // and 51089 millis after rollback.

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"
  val stateSize = 500000

  def main(args: Array[String]): Unit = {
    new File(targetDirectory).mkdirs()
    val threshold = args.headOption.getOrElse("1000").toInt
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark..")

    val settings = ErgoSettings.read(None)

    log.info(s"Settings to be used:\n$settings")

    val constants = StateConstants(None, settings)
    val emissionBox = genesisEmissionBox(constants.emission)

    val boxHolder = (0 to stateSize).map { i =>
      ErgoBox(120099L * i, Values.TrueLeaf, 1L, Seq.empty, Map.empty)
    } :+ emissionBox

    val additionalBoxes: Seq[Seq[ErgoBox]] = (0 to 1000).map { i =>
      ErgoBox(140000L * i, Values.TrueLeaf, 1L, Seq.empty, Map.empty)
    }.grouped(100).toSeq

    val prover = createProver(boxHolder, constants, settings, None)

    additionalBoxes.foreach { boxes =>
      boxes.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes)))
      prover.generateProofAndUpdateStorage()
    }

    implicit val hf: HF = Algos.hash
    val serializer = new BatchAVLProverSerializer[Digest32, HF]

    val result0 = elapsedTimeOf {
      serializer.slice(prover.prover())
    }

    val versions = prover.storage.rollbackVersions
    val versionToRollbackTo = versions.dropRight(7).last
    val rollbackResult = prover.rollback(versionToRollbackTo)

    log.info(rollbackResult.map(_ => s"Successfully rolled back to version ${Base16.encode(versionToRollbackTo)}")
      .getOrElse("Rollback failed"))

    val result = elapsedTimeOf {
      serializer.slice(prover.prover())
    }

    log.info(s"Elapsed time of taking snapshot before rollback is $result0 millis (${stateSize + 1002})")
    log.info(s"Elapsed time of taking snapshot after rollback is $result millis (~$stateSize)")

    System.exit(0)
  }

  private def createProver(boxes: Seq[ErgoBox],
                           constants: StateConstants,
                           settings: ErgoSettings,
                           nodeViewHolderRef: Option[ActorRef] = None): PersistentBatchAVLProver[Digest32, HF] = {
    log.info(s"Generating prover with ${boxes.size} boxes inside..")

    val p = new BatchAVLProver[Digest32, HF](keyLength = Constants.HashLength, valueLengthOpt = None)
    boxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).get)

    val nodeParameters = NodeParameters(keySize = Constants.HashLength, valueSize = None, labelSize = 32)
    val store = new LSMStore(TempDir.createTempDir, keepVersions = constants.keepVersions)
    val defaultStateContext = ErgoStateContext.empty(p.digest)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, nodeParameters)(Algos.hash)
    createPersistentProver(p, storage, ErgoState.genesisStateVersion, None, defaultStateContext)
  }

}

