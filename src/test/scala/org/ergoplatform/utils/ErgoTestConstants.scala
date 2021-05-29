package org.ergoplatform.utils

import akka.util.Timeout
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.{AutolykosPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateConstants, StateType, UpcomingStateContext}
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, ErgoProvingInterpreter}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.{DataInput, ErgoBox, ErgoScriptPredef}
import scorex.core.app.Version
import scorex.core.network.PeerSpec
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.Values.ErgoTree
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._

trait ErgoTestConstants extends ScorexLogging {

  implicit val votingSettings: VotingSettings = VotingSettings(1024, 32, 128, 32 * 1024, "01")
  val validationSettings: ErgoValidationSettings = ErgoValidationSettings.initial
  implicit val validationSettingsNoIl: ErgoValidationSettings = validationSettings
    .updated(ErgoValidationSettingsUpdate(Seq(exIlUnableToValidate, exIlEncoding, exIlStructure, exEmpty), Seq()))

  val parameters: Parameters = LaunchParameters
  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider
  val initSettings: ErgoSettings = ErgoSettings.read(Args(Some("src/test/resources/application.conf"), None))

  implicit val settings: ErgoSettings = initSettings

  val popowAlgos = new NipopowAlgos(powScheme)

  val lightModeSettings: ErgoSettings = initSettings.copy(
    nodeSettings = initSettings.nodeSettings.copy(stateType = StateType.Digest)
  )

  val emission: EmissionRules = settings.chainSettings.emissionRules
  val coinsTotal: Long = emission.coinsTotal
  val stateConstants: StateConstants = StateConstants(None, settings)
  val genesisStateDigest: ADDigest = settings.chainSettings.genesisStateDigest
  val feeProp: ErgoTree = ErgoScriptPredef.feeProposition(emission.settings.minerRewardDelay)

  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  lazy val defaultSeed: Array[Byte] = Mnemonic.toSeed(settings.walletSettings.testMnemonic.get)
  val defaultRootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(defaultSeed)
  val defaultChildSecrets: IndexedSeq[ExtendedSecretKey] = settings.walletSettings.testKeysQty
    .toIndexedSeq
    .flatMap(x => (0 until x).map(defaultRootSecret.child))
  val genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(settings.chainSettings)
  val genesisEmissionBox: ErgoBox = ErgoState.genesisBoxes(settings.chainSettings).head
  val defaultProver: ErgoProvingInterpreter = ErgoProvingInterpreter(
    defaultRootSecret +: defaultChildSecrets, parameters)
  val defaultMinerSecret: DLogProverInput = defaultProver.hdKeys.head.privateInput
  val defaultMinerSecretNumber: BigInt = defaultProver.hdKeys.head.privateInput.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage
  val defaultMinerPkPoint: EcPointType = defaultMinerPk.h

  val defaultTimestamp: Long = 1552217190000L
  val defaultNBits: Long = settings.chainSettings.initialNBits
  val defaultVotes: Array[Byte] = Array.fill(3)(0.toByte)
  val defaultVersion: Byte = 0
  lazy val powScheme: AutolykosPowScheme = settings.chainSettings.powScheme.ensuring(_.isInstanceOf[DefaultFakePowScheme])
  val emptyVSUpdate = ErgoValidationSettingsUpdate.empty
  val emptyStateContext: UpcomingStateContext = ErgoStateContext.empty(genesisStateDigest, settings)
    .upcoming(defaultMinerPkPoint, defaultTimestamp, defaultNBits, defaultVotes, emptyVSUpdate, defaultVersion)

  val startHeight: Int = emptyStateContext.currentHeight
  val startDigest: ADDigest = emptyStateContext.genesisStateDigest

  val EmptyStateRoot: ADDigest = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  val EmptyDigest32: Digest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new LinearDifficultyControl(settings.chainSettings)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(Array(0: Byte, 8: Byte) -> EmptyDigest32))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq())
  val emptyDataInputs: IndexedSeq[DataInput] = IndexedSeq()
  val emptyDataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()
  def emptyVerifier: ErgoInterpreter = ErgoInterpreter(emptyStateContext.currentParameters)

  val defaultTimeout: Timeout = Timeout(14.seconds)
  val defaultAwaitDuration: FiniteDuration = defaultTimeout.duration + 1.second

  val defaultPeerSpec = PeerSpec(
    settings.scorexSettings.network.agentName,
    Version(settings.scorexSettings.network.appVersion),
    settings.scorexSettings.network.nodeName,
    None,
    Seq.empty
  )

}
