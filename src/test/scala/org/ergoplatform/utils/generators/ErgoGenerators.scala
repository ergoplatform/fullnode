package org.ergoplatform.utils.generators

import com.google.common.primitives.Shorts
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import org.ergoplatform.mining.{AutolykosSolution, genPk, q}
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.{ADProofs, Extension, Header}
import org.ergoplatform.modifiers.mempool.TransactionIdsForHeader
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoTestConstants
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators
import scorex.util.{ModifierId, _}
import sigmastate.Values.{EvaluatedValue, FalseLeaf, TrueLeaf, Value}
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.ProverResult
import sigmastate.{SBoolean, _}

import scala.util.Random

trait ErgoGenerators extends CoreGenerators with Matchers with ErgoTestConstants {

  lazy val trueLeafGen: Gen[Value[SBoolean.type]] = Gen.const(TrueLeaf)
  lazy val falseLeafGen: Gen[Value[SBoolean.type]] = Gen.const(FalseLeaf)

  lazy val smallPositiveInt: Gen[Int] = Gen.choose(1, 5)

  lazy val noProofGen: Gen[ProverResult] =
    Gen.const(emptyProverResult)

  lazy val proveDlogGen: Gen[ProveDlog] = for {
    seed <- genBytes(32)
  } yield DLogProverInput(BigIntegers.fromUnsignedByteArray(seed)).publicImage

  lazy val ergoPropositionGen: Gen[Value[SBoolean.type]] = Gen.oneOf(trueLeafGen, falseLeafGen, proveDlogGen)

  lazy val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue)

  def validValueGen(proposition: Value[SBoolean.type],
                    additionalTokens: Seq[(TokenId, Long)] = Seq(),
                    additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map(),
                    transactionId: ModifierId = Array.fill[Byte](32)(0.toByte).toModifierId,
                    boxId: Short = 0,
                    creationHeight: Long = 0): Gen[Long] = {
    //there are outputs in tests of 183 bytes, and maybe in some tests at least 2 outputs are required
    //thus we put in an input a monetary value which is at least enough for storing 400 bytes of outputs
    val minValue = parameters.minValuePerByte * 400
    Gen.choose(minValue, coinsTotal)
  }

  lazy val ergoSyncInfoGen: Gen[ErgoSyncInfo] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(ErgoSyncInfo.MaxBlockIds))
  } yield ErgoSyncInfo(ids)

  lazy val transactionIdsForHeaderGen: Gen[TransactionIdsForHeader] = for {
    idGenerator <- genBytes(32)
    maxLength = 100
    toTake <- Gen.chooseNum(1, 100)
    ids <- Gen.listOfN(maxLength, idGenerator).map(_.take(toTake))
  } yield TransactionIdsForHeader(ids)

  lazy val digest32Gen: Gen[Digest32] = {
    val x = Digest32 @@ genBytes(32)
    x
  }

  lazy val boxIdGen: Gen[BoxId] = {
    val x = ADKey @@ genBytes(Constants.ModifierIdSize)
    x
  }

  lazy val stateRootGen: Gen[ADDigest] = {
    val x = ADDigest @@ genBytes(Constants.ModifierIdSize + 1)
    x
  }

  lazy val serializedAdProofGen: Gen[SerializedAdProof] = {
    val x = SerializedAdProof @@ genBoundedBytes(32, 32 * 1024)
    x
  }

  def genSecureBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen.choose(minSize, maxSize).flatMap {
      scorex.util.Random.randomBytes
    }

  def extensionKvGen(keySize: Int, valuesSize: Int): Gen[(Array[Byte], Array[Byte])] = for {
    key <- genSecureBoundedBytes(keySize, keySize)
    value <- if(key.head == 0) genSecureBoundedBytes(4, 4) else genSecureBoundedBytes(valuesSize, valuesSize)
  } yield (key, value)

  lazy val extensionGen: Gen[Extension] = for {
    headerId <- modifierIdGen
    mandatoryElements <- Gen.mapOf(extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize))
  } yield {
    val me = mandatoryElements
      .map(kv => Shorts.fromByteArray(kv._1) -> kv._2)
      .map(kv => Shorts.toByteArray(kv._1) -> kv._2)
    Extension(headerId, me.toSeq)
  }

  lazy val genECPoint: Gen[EcPointType] = genBytes(32).map(b => genPk(BigInt(b).mod(q)))

  lazy val powSolutionGen: Gen[AutolykosSolution] = for {
    pk <- genECPoint
    w <- genECPoint
    n <- genBytes(8)
    d <- Arbitrary.arbitrary[BigInt].map(_.mod(q))
  } yield AutolykosSolution(pk, w, n, d)

  /**
    * Header generator with default miner pk in pow solution
    */
  lazy val defaultHeaderGen: Gen[Header] = invalidHeaderGen.map{ h =>
    h.copy(powSolution = h.powSolution.copy(pk = defaultMinerPkPoint))
  }

  lazy val invalidHeaderGen: Gen[Header] = for {
    version <- Arbitrary.arbitrary[Byte]
    parentId <- modifierIdGen
    stateRoot <- stateRootGen
    adRoot <- digest32Gen
    transactionsRoot <- digest32Gen
    requiredDifficulty <- Arbitrary.arbitrary[BigInt]
    height <- Gen.choose(1, Int.MaxValue)
    powSolution <- powSolutionGen
    interlinks <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(128))
    timestamp <- positiveLongGen
    extensionHash <- digest32Gen
  } yield Header(
    version,
    parentId,
    interlinks,
    adRoot,
    stateRoot,
    transactionsRoot,
    timestamp,
    RequiredDifficulty.encodeCompactBits(requiredDifficulty),
    height,
    extensionHash,
    powSolution,
    Array.fill(3)(0: Byte),
    None
  )
  lazy val randomADProofsGen: Gen[ADProofs] = for {
    headerId <- modifierIdGen
    proof <- serializedAdProofGen
  } yield ADProofs(headerId, proof)

  lazy val emptyMemPoolGen: Gen[ErgoMemPool] = {
    Gen.resultOf({ _: Unit => ErgoMemPool.empty(settings) })(Arbitrary(Gen.const(())))
  }

  /** Random long from 1 to maximum - 1
    *
    * @param maximum should be positive
    */
  def randomLong(maximum: Long = Long.MaxValue): Long = {
    if (maximum < 3) 1 else Math.abs(Random.nextLong()) % (maximum - 2) + 1
  }

}
