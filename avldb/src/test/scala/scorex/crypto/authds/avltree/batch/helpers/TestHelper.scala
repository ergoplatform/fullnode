package scorex.crypto.authds.avltree.batch.helpers

import io.iohk.iodb.{LSMStore, QuickStore, Store}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.util.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ScorexLogging

trait TestHelper extends FileHelper with ScorexLogging {

  val enableQuickStore: Boolean = System.getProperty("java.specification.version").startsWith("8")

  def quickTest[R](block: => R): Option[R] = if(enableQuickStore) Some(block)
  else None

  type HF = Blake2b256.type
  type D = Digest32
  type AD = ADDigest
  type P = SerializedAdProof
  type PROVER = BatchAVLProver[D, HF]
  type VERIFIER = BatchAVLVerifier[D, HF]
  type PERSISTENT_PROVER = PersistentBatchAVLProver[D, HF]
  type STORAGE = VersionedIODBAVLStorage[D]

  protected val KL: Int
  protected val VL: Int
  protected val LL: Int

  implicit val hf: HF = Blake2b256

  case class Data(p: PERSISTENT_PROVER, s: STORAGE)

  def createLSMStore(keepVersions: Int = 0): Store = {
    val dir = getRandomTempDir
    new LSMStore(dir, keepVersions = keepVersions)
  }

  def createQuickStore(keepVersions: Int = 0): Store = {
    val dir = getRandomTempDir
    new QuickStore(dir, keepVersions = keepVersions)
  }

  def createVersionedStorage(store: Store): STORAGE = new VersionedIODBAVLStorage(store, NodeParameters(KL, Some(VL), LL))

  def createPersistentProver(storage: STORAGE): PERSISTENT_PROVER = {
    val prover = new BatchAVLProver[D, HF](KL, Some(VL))
    createPersistentProver(storage, prover)
  }

  def createPersistentProver(storage: STORAGE, prover: PROVER): PERSISTENT_PROVER =
    PersistentBatchAVLProver.create[D, HF](prover, storage, paranoidChecks = true).get

  def createPersistentProverWithLSM(keepVersions: Int = 0): PERSISTENT_PROVER = {
    val store = createLSMStore(keepVersions)
    val storage = createVersionedStorage(store)
    createPersistentProver(storage)
  }

  def createPersistentProverWithQuick(keepVersions: Int = 0): PERSISTENT_PROVER = {
    val store = createQuickStore(keepVersions)
    val storage = createVersionedStorage(store)
    createPersistentProver(storage)
  }

  def createVerifier(digest: AD, proof: P): VERIFIER = new BatchAVLVerifier[D, HF](digest, proof, KL, Some(VL))


  implicit class DigestToBase58String(d: ADDigest) {

    def toBase58: String = Base58.encode(d)
  }

}
