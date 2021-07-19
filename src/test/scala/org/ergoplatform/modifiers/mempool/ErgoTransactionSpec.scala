package org.ergoplatform.modifiers.mempool

import io.circe.syntax._
import org.ergoplatform.ErgoBox._
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{ErgoStateContext, UpcomingStateContext, VotingData}
import org.ergoplatform.settings.Parameters.MaxBlockCostIncrease
import org.ergoplatform.settings.ValidationRules.{bsBlockTransactionsCost, txBoxSize}
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.{InputContext, TransactionContext}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.ByteArrayWrapper
import scorex.util.encode.Base16
import sigmastate.AND
import sigmastate.Values.{ByteArrayConstant, ByteConstant, IntConstant, LongArrayConstant, SigmaPropConstant, TrueLeaf}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, CryptoConstants, ProverResult}
import sigmastate.utxo.CostTable
import sigmastate.helpers.TestingHelpers._

import scala.util.{Random, Try}

class ErgoTransactionSpec extends ErgoPropertyTest {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("serialization vector") {
    // test vectors, that specifies transaction json and bytes representation.
    // ensures that bytes transaction representation was not changed

    def check(bytesStr: String, bytesToSign: String, jsonStr: String): Unit = {
      val bytes = Base16.decode(bytesStr).get
      val tx = ErgoTransactionSerializer.parseBytes(bytes)
      tx.asJson.noSpaces shouldBe jsonStr
      bytesToSign shouldBe Base16.encode(tx.messageToSign)
    }

    // simple transfer transaction with 2 inputs and 2 outputs (first is transfer, second is fee)
    val height = 1000
    val minerPkHex = "0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942"
    val minerPk = Base16.decode(minerPkHex).map { point =>
      ProveDlog(
        CryptoConstants.dlogGroup.curve.decodePoint(point).asInstanceOf[CryptoConstants.EcPointType]
      )
    }.get
    val inputs: IndexedSeq[Input] = IndexedSeq(
      new Input(ADKey @@ Base16.decode("c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742").get,
        new ProverResult(Base16.decode("b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674").get, new ContextExtension(Map()))),
      new Input(ADKey @@ Base16.decode("ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead").get,
        new ProverResult(Base16.decode("5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340").get, new ContextExtension(Map()))),
    )
    val outputCandidates: IndexedSeq[ErgoBoxCandidate] = IndexedSeq(
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl, Map()),
      new ErgoBoxCandidate(1000000L, settings.chainSettings.monetary.feeProposition, height, Colls.emptyColl, Map())
    )
    val tx = ErgoTransaction(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate])

    val bytesToSign = "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f77420000ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead00000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000c0843d1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304e8070000"
    val bytesStr = "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f774238b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de467400ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead385aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000c0843d1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304e8070000"
    val jsonStr = "{\"id\":\"b59ca51f7470f291acc32e84870d00c4fda8b773f38f757f3d65d45265c13da5\",\"inputs\":[{\"boxId\":\"c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742\",\"spendingProof\":{\"proofBytes\":\"b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674\",\"extension\":{}}},{\"boxId\":\"ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead\",\"spendingProof\":{\"proofBytes\":\"5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340\",\"extension\":{}}}],\"dataInputs\":[],\"outputs\":[{\"boxId\":\"da288ce9e9a9d39f69634488a8d82c1bf4fb6ddce2f0930d2536016d8167eeb2\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{},\"transactionId\":\"b59ca51f7470f291acc32e84870d00c4fda8b773f38f757f3d65d45265c13da5\",\"index\":0},{\"boxId\":\"be609af4436111d5592dbd52bc64f6a46a1c0605fd30cd61c74850b7f9875762\",\"value\":1000000,\"ergoTree\":\"1005040004000e36100204cf0f08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{},\"transactionId\":\"b59ca51f7470f291acc32e84870d00c4fda8b773f38f757f3d65d45265c13da5\",\"index\":1}],\"size\":341}"
    Base16.encode(tx.bytes) shouldBe bytesStr

    check(bytesStr, bytesToSign, jsonStr)

    // tx with registers in outputs
    val outputCandidates2: IndexedSeq[ErgoBoxCandidate] = IndexedSeq(
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl,
        Map(
          R6 -> IntConstant(10),
          R4 -> ByteConstant(1),
          R5 -> SigmaPropConstant(minerPk),
          R7 -> LongArrayConstant(Array(1L, 2L, 1234123L)),
          R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        )),
      new ErgoBoxCandidate(1000000000L, minerPk, height, Colls.emptyColl, Map())
    )
    val tx2 = ErgoTransaction(inputs: IndexedSeq[Input], outputCandidates2: IndexedSeq[ErgoBoxCandidate])

    Base16.encode(tx2.bytes) shouldBe "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f774238b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de467400ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead385aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070005020108cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b94204141103020496d396010e211234561234561234561234561234561234561234561234561234561234561234568094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000"
    check(Base16.encode(tx2.bytes), "02c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f77420000ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead00000000028094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070005020108cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b94204141103020496d396010e211234561234561234561234561234561234561234561234561234561234561234568094ebdc030008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942e8070000",
      "{\"id\":\"bd04a93f67fda77d89afc38cd8237f142ad5a349405929fd1f7b7f24c4ea2e80\",\"inputs\":[{\"boxId\":\"c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742\",\"spendingProof\":{\"proofBytes\":\"b4a04b4201da0578be3dac11067b567a73831f35b024a2e623c1f8da230407f63bab62c62ed9b93808b106b5a7e8b1751fa656f4c5de4674\",\"extension\":{}}},{\"boxId\":\"ca796a4fc9c0d746a69702a77bd78b1a80a5ef5bf5713bbd95d93a4f23b27ead\",\"spendingProof\":{\"proofBytes\":\"5aea4d78a234c35accacdf8996b0af5b51e26fee29ea5c05468f23707d31c0df39400127391cd57a70eb856710db48bb9833606e0bf90340\",\"extension\":{}}}],\"dataInputs\":[],\"outputs\":[{\"boxId\":\"1baffa8e5ffce634a8e70530023c16a5c177d2b5ab756ae89a8dce2a23ba433c\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{\"R4\":\"0201\",\"R5\":\"08cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"R6\":\"0414\",\"R7\":\"1103020496d39601\",\"R8\":\"0e21123456123456123456123456123456123456123456123456123456123456123456\"},\"transactionId\":\"bd04a93f67fda77d89afc38cd8237f142ad5a349405929fd1f7b7f24c4ea2e80\",\"index\":0},{\"boxId\":\"33eff46f94067b32073d5f81984607be559108f58bc3f53906a1e8db7cf0f708\",\"value\":1000000000,\"ergoTree\":\"0008cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942\",\"assets\":[],\"creationHeight\":1000,\"additionalRegisters\":{},\"transactionId\":\"bd04a93f67fda77d89afc38cd8237f142ad5a349405929fd1f7b7f24c4ea2e80\",\"index\":1}],\"size\":356}")
//
    // tx with 2 inputs, 1 data input, 3 outputs with tokens 0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942
    check("02e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6400002e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9000001f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195026df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6403da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100fa979af8988ce7010008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000000",
      "02e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6400002e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9000001f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195026df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c6403da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100da92a8b8e3ad770008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000200daa4eb6b01aec8d1ff0100fa979af8988ce7010008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176000000",
      "{\"id\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"inputs\":[{\"boxId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}},{\"boxId\":\"2e9798d7eb0cd867f6dc29872f80de64c04cef10a99a58d007ef7855f0acbdb9\",\"spendingProof\":{\"proofBytes\":\"\",\"extension\":{}}}],\"dataInputs\":[{\"boxId\":\"f97d1dc4626de22db836270fe1aa004b99970791e4557de8f486f6d433b81195\"}],\"outputs\":[{\"boxId\":\"69e05b68715caaa4ca58ba59a8c8c7e031d42ad890b05f87021a28617c1e70d5\",\"value\":524940416256346,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{},\"transactionId\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"index\":0},{\"boxId\":\"556a9a3ec7880d468e56d44e75898cf8a32f6a07344895fa6b5cf34edf101a59\",\"value\":524940416256346,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[{\"tokenId\":\"6df03fffc9042bf0edb0d0d36d7a675239b83a9080d39716b9aa0a64cccb9963\",\"amount\":226153050},{\"tokenId\":\"e76bf387ab2e63ba8f4e23267bc88265b5fee4950030199e2e2c214334251c64\",\"amount\":536110126}],\"creationHeight\":0,\"additionalRegisters\":{},\"transactionId\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"index\":1},{\"boxId\":\"16385b5b83992629909c7e004ed0421229ed3587162ce6f29b2df129472e3909\",\"value\":1016367755463674,\"ergoTree\":\"0008cd02db0ce4d301d6dc0b7a5fbe749588ef4ef68f2c94435020a3c31764ffd36a2176\",\"assets\":[],\"creationHeight\":0,\"additionalRegisters\":{},\"transactionId\":\"663ae91ab7145a4f42b5509e1a2fb0469b7cb46ea87fdfd90e0b4c8ef29c2493\",\"index\":2}],\"size\":329}")
  }

  property("31-bytes long z value") {
    /*
box_id;tx_id;index;proof_bytes
e4c066dc019767daf05266eb6f09fa1dd833422f199842582ba13dc7bdb651a8;d5942d536924d54178e95a06ef5256a9768b912332a659b7f1c285752e8b4ceb;0;c18ac0240928432db15dc915879583cb7ad7a634f3807073b294f0670c1d6f25814dadbbe7afe986893c7e8660dd3c4e86c1b1c5e029a8
feb39a66c38677816d08a59e44f210d9942811e12569495bab856664f229cc15;f522ebac19d09232c370101792099c80d83b4877841ca883625717d0aabf3b1a;0;36110092d1bf698cd27b57d6eaa4ac32988fdf5a1c0b3a3090776908126127bb8e27a5f9ebbcb758f85a9eed413f549a10a634713105a1
660dda9410dc07df6cf86f839ec879562aa38aea499bc2692141b343bfcc5d06;9285ff71d182bef1dc18fb1dc55dd14eaf4d34ad716e16da2baf0a3e75b42e79;0;d6d79c0b95a1764f42275c87e191e8a5e1c6e8cf49680702bdb99d7b5ea628aacbd00eb22b2e04d45f8c789f95bdc99a509bf0427234ba
c296b61c04c2b0acadf3c6a6c7a3b658314ce60bc0fe78bd1e5fc6581d5b4d91;c64a71ccc3b9c6141711ec931c9544fc2f6c6d47872bd6d3cde24158a820bc68;0;1ebf1a24091ab89494e5f7e44445c91e94a0090516ce74e9a12d75eb30ac62fd047ac2e841217c1667034045a845bb8b67d489c12e851f
3b0065d3ec18aecf7951b5fd26c3344731fe7625fcb223b46480c2951cfaa4fc;9f4d40607a561c14a504c96919362f150fca3337ab43219d9a683f1617b74b0a;0;6d4ae886951e35c04f328557cb3e1b3aa7376fa68a30e466bfb1afcb318f72785741ad035399bf3b683f062647966a547f6c4978a887d0
aa9428a1709e1b35825145b9f331c06c9fcbf5ff8a270102e7cd279beeea1acd;c78f0757e946c97ad75672b4f0d2f56e6fc7e748360c6129a016edae8365b6af;0;f6f076859438d39dcd977e7e403c9b1362a0b51dfa9fc2e1c996d4413354c9b1a24bae220018c135d18611720ac3918f21f6f62740945a
88005d9e557e34fbc50a0c1e259d85fe81ff7aef78457f6717b9dea3d86e3b1c;146020cd5e8d2a5e0863a1a50477bd5f96459e3c8509f8cc048071086046c5af;0;6a5f7e7ee68762b61a0e8b64e353f66d0be523b7bf8d56b662112ec47e32ec7183525c9851a608885a34051bc971d6c8600c88d8ce1713
5242e0d7651df29834aa4138639b7db8d84c1a6c252e7ee14fd6fe3db7f6f664;f55ccfe3d56cd9f1e6510cd3bc726ae5a04bf903ce2a5a5602cd4d4175ebf1e1;0;84c2b4f8ddc9b2a16250fd9f104ca3c080fb15f77acffe5f1a2521bd4ab522e4820207943f68c2705b8de04e3559601d2a4dc7c701f462
048d53c9c636702368bc1bf0f3e338ae2c1c78dab854d71d50e0aea58a1ed24d;4a98cb8d6568d2c40949c0f2197dc3bfdaa05b3a8009beb20e7c14c0782da9cc;0;d7ba05a6d843b2a781ea52f3f2e022644bae928631e3ed43dc220550716d44b84c0933c056d60b5a0becd72fe21b9fc5631ca658ad20f2
 */
    import io.circe.parser._

    val ts = """{
               |      "id": "d5942d536924d54178e95a06ef5256a9768b912332a659b7f1c285752e8b4ceb",
               |      "inputs": [
               |        {
               |          "boxId": "e4c066dc019767daf05266eb6f09fa1dd833422f199842582ba13dc7bdb651a8",
               |          "spendingProof": {
               |            "proofBytes": "c18ac0240928432db15dc915879583cb7ad7a634f3807073b294f0670c1d6f25814dadbbe7afe986893c7e8660dd3c4e86c1b1c5e029a8",
               |            "extension": {}
               |          }
               |        }
               |      ],
               |      "dataInputs": [],
               |      "outputs": [
               |        {
               |          "boxId": "8b3ab6afc11f484546fc5d473d99fedd77331c632df3ac2a154af7412dceded7",
               |          "value": 50000000000,
               |          "ergoTree": "0008cd023276b8f770a45a55af95ca36ed79d2229b8567f09e04e3afdf459ab53c36f2fe",
               |          "assets": [],
               |          "creationHeight": 335388,
               |          "additionalRegisters": {},
               |          "transactionId": "d5942d536924d54178e95a06ef5256a9768b912332a659b7f1c285752e8b4ceb",
               |          "index": 0
               |        },
               |        {
               |          "boxId": "06f6e11219b1b27a262eb709a96e7d91ffd9316e419a60792bcb087a2af0dd11",
               |          "value": 505063046902470,
               |          "ergoTree": "0008cd02f40ee9cecf47e36a18645e5a3cff677772d74ee2792912173aa406fa9a8a2ef4",
               |          "assets": [],
               |          "creationHeight": 335388,
               |          "additionalRegisters": {},
               |          "transactionId": "d5942d536924d54178e95a06ef5256a9768b912332a659b7f1c285752e8b4ceb",
               |          "index": 1
               |        },
               |        {
               |          "boxId": "88c1ea9d5634ee85713d45897732414d2f6375bc9dd9274e05110cf8c13c52a6",
               |          "value": 1000000,
               |          "ergoTree": "1005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304",
               |          "assets": [],
               |          "creationHeight": 335388,
               |          "additionalRegisters": {},
               |          "transactionId": "d5942d536924d54178e95a06ef5256a9768b912332a659b7f1c285752e8b4ceb",
               |          "index": 2
               |        }
               |      ],
               |      "size": 301
               |    }""".stripMargin
    val t = ErgoTransaction.transactionDecoder.decodeJson(parse(ts).toOption.get).toOption.get

    val is = """{
               |          "boxId": "e4c066dc019767daf05266eb6f09fa1dd833422f199842582ba13dc7bdb651a8",
               |          "value": 505113047902470,
               |          "ergoTree": "0008cd02f40ee9cecf47e36a18645e5a3cff677772d74ee2792912173aa406fa9a8a2ef4",
               |          "assets": [],
               |          "creationHeight": 335377,
               |          "additionalRegisters": {},
               |          "transactionId": "3f788e8e31f11e2cd2ec9bc9df1f562caa177d4696f91d4f8cecda7fc3d2c3bd",
               |          "index": 1
               |        }""".stripMargin

    val i = ErgoTransaction.ergoBoxDecoder.decodeJson(parse(is).toOption.get).toOption.get

    implicit lazy val context: IRContext = new RuntimeIRContext
    val verifier = new ErgoInterpreter(parameters)

    val input = t.inputs.head
    val proof = input.spendingProof
    val transactionContext = TransactionContext(IndexedSeq(i), IndexedSeq.empty, t)
    val inputContext = InputContext(0.toShort, proof.extension)

    val ctx = new ErgoContext(emptyStateContext, transactionContext, inputContext, costLimit = 10000000, initCost = 0)

    verifier.verify(i.ergoTree, ctx, proof, t.messageToSign).get._1 shouldBe true
  }

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {
    forAll(validErgoTransactionGen, smallPositiveInt) { case ((from, tx), deltaAbs) =>
      val delta = if (Random.nextBoolean()) -deltaAbs else deltaAbs

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, delta) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess &&
        wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to create a negative-value output") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val negValue = Math.min(Math.abs(Random.nextLong()), Long.MaxValue - tx.outputCandidates.head.value)
      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, -(tx.outputCandidates.head.value + negValue)) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  property("impossible to overflow ergo tokens") {
    forAll(validErgoTransactionGen) { case (from, tx) =>
      val overflowSurplus = (Long.MaxValue - tx.outputCandidates.map(_.value).sum) + 1

      val wrongTx = tx.copy(outputCandidates =
        modifyValue(tx.outputCandidates.head, overflowSurplus) +: tx.outputCandidates.tail)

      wrongTx.statelessValidity().isSuccess shouldBe false
      wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext).isSuccess shouldBe false
    }
  }

  private def updateAnAsset(tx: ErgoTransaction, from: IndexedSeq[ErgoBox], deltaFn: Long => Long) = {
    val updCandidates = tx.outputCandidates.foldLeft(IndexedSeq[ErgoBoxCandidate]() -> false) { case ((seq, modified), ebc) =>
      if (modified) {
        (seq :+ ebc) -> true
      } else {
        if (ebc.additionalTokens.nonEmpty && ebc.additionalTokens.exists(t => !java.util.Arrays.equals(t._1, from.head.id))) {
          (seq :+ modifyAsset(ebc, deltaFn, Digest32 @@ from.head.id)) -> true
        } else {
          (seq :+ ebc) -> false
        }
      }
    }._1
    tx.copy(outputCandidates = updCandidates)
  }

  property("assets preservation law holds") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ + 1)) shouldBe 'failure
    }
  }

  property("impossible to create an asset of non-positive amount") {
    forAll(validErgoTransactionWithAssetsGen) { case (from, tx) =>
      checkTx(from, updateAnAsset(tx, from, _ => -1)) shouldBe 'failure
    }
  }

  property("impossible to overflow an asset value") {
    val gen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(gen) { case (from, tx) =>
      val tokenOpt = tx.outputCandidates.flatMap(_.additionalTokens.toArray).map(t => ByteArrayWrapper.apply(t._1) -> t._2)
        .groupBy(_._1).find(_._2.size >= 2)

      whenever(tokenOpt.nonEmpty) {
        val tokenId = tokenOpt.get._1
        val tokenAmount = tokenOpt.get._2.map(_._2).sum

        var modified = false
        val updCandidates = tx.outputCandidates.map { c =>
          val updTokens = c.additionalTokens.map { case (id, amount) =>
            if (!modified && ByteArrayWrapper(id) == tokenId) {
              modified = true
              id -> ((Long.MaxValue - tokenAmount) + amount + 1)
            } else {
              id -> amount
            }
          }
          new ErgoBoxCandidate(c.value, c.ergoTree, startHeight, updTokens, c.additionalRegisters)
        }

        val wrongTx = tx.copy(outputCandidates = updCandidates)
        checkTx(from, wrongTx) shouldBe 'failure
      }
    }
  }

  property("stateful validation should catch false proposition") {
    val propositionGen = Gen.const(Constants.FalseLeaf)
    val gen = validErgoTransactionGenTemplate(1, 1, 1, 1, propositionGen)
    forAll(gen) { case (from, tx) =>
      tx.statelessValidity().isSuccess shouldBe true
      val validity = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext)
      validity.isSuccess shouldBe false
      val e = validity.failed.get
      e.getMessage should startWith(ValidationRules.errorMessage(ValidationRules.txScriptValidation, ""))
    }
  }

  property("assets usage correctly affects transaction total cost") {
    val txGen = validErgoTransactionGenTemplate(1, 1, 8, 16)
    forAll(txGen) { case (from, tx) =>
      val initTxCost = tx.statefulValidity(from, emptyDataBoxes, emptyStateContext).get

      // already existing token from one of the inputs
      val existingToken = from.flatMap(_.additionalTokens.toArray).toSet.head
      // completely new token
      val randomToken = (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong)

      val in0 = from.last
      // new token added to the last input
      val modifiedIn0 = testBox(in0.value, in0.ergoTree, in0.creationHeight,
        in0.additionalTokens.toArray.toSeq :+ randomToken, in0.additionalRegisters, in0.transactionId, in0.index)
      val txInMod0 = tx.inputs.last.copy(boxId = modifiedIn0.id)

      val in1 = from.last
      // existing token added to the last input
      val modifiedIn1 = testBox(in1.value, in1.ergoTree, in1.creationHeight,
        in1.additionalTokens.toArray.toSeq :+ existingToken, in1.additionalRegisters, in1.transactionId, in1.index)
      val txInMod1 = tx.inputs.last.copy(boxId = modifiedIn1.id)

      val out0 = tx.outputs.last
      // new token added to the last output
      val modifiedOut0 = testBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens.toArray.toSeq :+ randomToken, out0.additionalRegisters, out0.transactionId, out0.index)
      // existing token added to the last output
      val modifiedOut1 = testBox(out0.value, out0.ergoTree, out0.creationHeight,
        out0.additionalTokens.toArray.toSeq :+ existingToken, out0.additionalRegisters, out0.transactionId, out0.index)

      // update transaction inputs and outputs accordingly
      val txMod0 = tx.copy(inputs = tx.inputs.init :+ txInMod0) // new token group added to one input
    val txMod1 = tx.copy(inputs = tx.inputs.init :+ txInMod1) // existing token added to one input
    val txMod2 = tx.copy(inputs = tx.inputs.init :+ txInMod0, // new token group added to one input and one output
      outputCandidates = tx.outputCandidates.init :+ modifiedOut0)
      val txMod3 = tx.copy(inputs = tx.inputs.init :+ txInMod1, // existing token added to one input and one output
        outputCandidates = tx.outputCandidates.init :+ modifiedOut1)

      val inputIncTxCost0 = txMod0.statefulValidity(from.init :+ modifiedIn0, emptyDataBoxes, emptyStateContext).get
      val inputIncTxCost1 = txMod1.statefulValidity(from.init :+ modifiedIn1, emptyDataBoxes, emptyStateContext).get
      val outputIncTxCost0 = txMod2.statefulValidity(from.init :+ modifiedIn0, emptyDataBoxes, emptyStateContext).get
      val outputIncTxCost1 = txMod3.statefulValidity(from.init :+ modifiedIn1, emptyDataBoxes, emptyStateContext).get

      (inputIncTxCost0 - initTxCost) shouldEqual Parameters.TokenAccessCostDefault * 2 // one more group + one more token in total
      (inputIncTxCost1 - initTxCost) shouldEqual Parameters.TokenAccessCostDefault // one more token in total
      (outputIncTxCost0 - inputIncTxCost0) shouldEqual Parameters.TokenAccessCostDefault * 2
      (outputIncTxCost1 - inputIncTxCost1) shouldEqual Parameters.TokenAccessCostDefault
    }
  }

  property("spam simulation (transaction validation cost with too many tokens exceeds block limit)") {
    val bxsQty = 400
    val (inputs, tx) = validErgoTransactionGenTemplate(1, 1, 8, 16).sample.get // it takes too long to test with `forAll`
    val tokens = (0 until 255).map(_ => (Digest32 @@ scorex.util.Random.randomBytes(), Random.nextInt(100000000).toLong))
    val (in, out) = {
      val in0 = inputs.head
      val out0 = tx.outputs.head
      val inputsMod = (0 until bxsQty).map { i =>
        testBox(10000000000L, in0.ergoTree, in0.creationHeight,
          tokens, in0.additionalRegisters, in0.transactionId, i.toShort)
      }
      val outputsMod = (0 until bxsQty).map { i =>
        testBox(10000000000L, out0.ergoTree, out0.creationHeight,
          tokens, out0.additionalRegisters, out0.transactionId, i.toShort)
      }
      inputsMod -> outputsMod
    }
    val inputsPointers = {
      val inSample = tx.inputs.head
      (0 until bxsQty).map(i => inSample.copy(boxId = in(i).id))
    }
    val txMod = tx.copy(inputs = inputsPointers, outputCandidates = out)
    val validFailure = txMod.statefulValidity(in, emptyDataBoxes, emptyStateContext)
    validFailure.failed.get.getMessage should startWith(ValidationRules.errorMessage(txBoxSize, "").take(30))

  }

  property("transaction with too many inputs should be rejected") {

    //we assume that verifier must finish verification of any script in less time than 250K hash calculations
    // (for the Blake2b256 hash function over a single block input)
    val Timeout: Long = {
      val hf = Blake2b256

      //just in case to heat up JVM
      (1 to 5000000).foreach(i => hf(s"$i-$i"))

      val t0 = System.currentTimeMillis()
      (1 to 250000).foreach(i => hf(s"$i"))
      val t = System.currentTimeMillis()
      t - t0
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 1500, 2000, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity().isSuccess shouldBe true

    //check that spam transaction is being rejected quickly
    implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)
    val (validity, time0) = BenchmarkUtil.measureTime(tx.statefulValidity(from, IndexedSeq(), emptyStateContext))
    validity.isSuccess shouldBe false
    assert(time0 <= Timeout)

    val cause = validity.failed.get.getMessage
    cause should startWith(ValidationRules.errorMessage(bsBlockTransactionsCost, "").take(30))

    //check that spam transaction validation with no cost limit is indeed taking too much time
    import Parameters._
    val ps = Parameters(0, DefaultParameters.updated(MaxBlockCostIncrease, Int.MaxValue), emptyVSUpdate)
    val sc = new ErgoStateContext(Seq.empty, None, genesisStateDigest, ps, ErgoValidationSettings.initial,
      VotingData.empty)(settings)
      .upcoming(org.ergoplatform.mining.group.generator,
        0L,
        settings.chainSettings.initialNBits,
        Array.fill(3)(0.toByte),
        ErgoValidationSettingsUpdate.empty,
        0.toByte)
    val (_, time) = BenchmarkUtil.measureTime(
      tx.statefulValidity(from, IndexedSeq(), sc)(verifier)
    )

    assert(time > Timeout)
  }

  property("transaction cost") {
    def stateContextWithMaxCost(manualCost: Int): UpcomingStateContext = {
      val table2: Map[Byte, Int] = Parameters.DefaultParameters + (MaxBlockCostIncrease -> manualCost)
      val params2 = new Parameters(height = 0,
        parametersTable = table2,
        proposedUpdate = ErgoValidationSettingsUpdate.empty)
      emptyStateContext.copy(currentParameters = params2)(settings)
    }

    val gen = validErgoTransactionGenTemplate(0, 0, 10, 10, trueLeafGen)
    val (from, tx) = gen.sample.get
    tx.statelessValidity().isSuccess shouldBe true

    // calculate costs manually
    val initialCost: Long =
      tx.inputs.size * LaunchParameters.inputCost +
        tx.dataInputs.size * LaunchParameters.dataInputCost +
        tx.outputs.size * LaunchParameters.outputCost +
        CostTable.interpreterInitCost
    val (outAssets, outAssetsNum) = tx.outAssetsTry.get
    val (inAssets, inAssetsNum) = ErgoTransaction.extractAssets(from).get
    val totalAssetsAccessCost = (outAssetsNum + inAssetsNum) * LaunchParameters.tokenAccessCost +
      (inAssets.size + outAssets.size) * LaunchParameters.tokenAccessCost
    val scriptsValidationCosts = tx.inputs.size * (CostTable.constCost + CostTable.logicCost + CostTable.logicCost + from.head.ergoTree.complexity)
    val manualCost: Int = (initialCost + totalAssetsAccessCost + scriptsValidationCosts).toInt


    // check that validation pass if cost limit equals to manually calculated cost
    val sc = stateContextWithMaxCost(manualCost)
    sc.currentParameters.maxBlockCost shouldBe manualCost
    val calculatedCost = tx.statefulValidity(from, IndexedSeq(), sc)(ErgoInterpreter(sc.currentParameters)).get
    manualCost shouldBe calculatedCost

    // transaction exceeds computations limit
    val sc2 = stateContextWithMaxCost(manualCost - 1)
    tx.statefulValidity(from, IndexedSeq(), sc2)(ErgoInterpreter(sc2.currentParameters)) shouldBe 'failure

    // transaction exceeds computations limit due to non-zero accumulated cost
    tx.statefulValidity(from, IndexedSeq(), sc, 1)(ErgoInterpreter(sc.currentParameters)) shouldBe 'failure
  }

  property("cost accumulated correctly across inputs") {
    val accInitCost = 100000

    def inputCost(tx: ErgoTransaction, idx: Short, from: IndexedSeq[ErgoBox]): Long = {
      val idx = 0
      val input = tx.inputs(idx)
      val proof = input.spendingProof
      val transactionContext = TransactionContext(from, IndexedSeq(), tx)
      val inputContext = InputContext(idx.toShort, proof.extension)

      val ctx = new ErgoContext(
        emptyStateContext, transactionContext, inputContext,
        costLimit = emptyStateContext.currentParameters.maxBlockCost,
        initCost = 0)

      val messageToSign = tx.messageToSign

      val inputCost = verifier.verify(from(idx).ergoTree, ctx, proof, messageToSign).get._2

      inputCost
    }

    forAll(smallPositiveInt) { inputsNum =>

      val nonTrivialTrueGen = Gen.const(AND(Seq(TrueLeaf, TrueLeaf)).toSigmaProp.treeWithSegregation)
      val gen = validErgoTransactionGenTemplate(0, 0, inputsNum, inputsNum, nonTrivialTrueGen)
      val (from, tx) = gen.sample.get
      tx.statelessValidity().isSuccess shouldBe true

      tx.inputs.length shouldBe inputsNum

      val tokenAccessCost = emptyStateContext.currentParameters.tokenAccessCost

      val txCost = tx.statefulValidity(from, IndexedSeq(), emptyStateContext, accInitCost).get

      val (inAssets, inAssetsNum): (Map[ByteArrayWrapper, Long], Int) = ErgoTransaction.extractAssets(from).get
      val (outAssets, outAssetsNum): (Map[ByteArrayWrapper, Long], Int) = ErgoTransaction.extractAssets(tx.outputs).get

      val assetsCost = inAssetsNum * tokenAccessCost + inAssets.size * tokenAccessCost +
        outAssetsNum * tokenAccessCost + outAssets.size * tokenAccessCost

      val initialCost: Long =
        tx.inputs.size * LaunchParameters.inputCost +
          tx.dataInputs.size * LaunchParameters.dataInputCost +
          tx.outputs.size * LaunchParameters.outputCost +
          CostTable.interpreterInitCost +
          assetsCost

      txCost shouldBe (accInitCost + initialCost + inputCost(tx, 0, from) * inputsNum)
    }
  }

  private def modifyValue(boxCandidate: ErgoBoxCandidate, delta: Long): ErgoBoxCandidate = {
    new ErgoBoxCandidate(
      boxCandidate.value + delta,
      boxCandidate.ergoTree,
      boxCandidate.creationHeight,
      boxCandidate.additionalTokens,
      boxCandidate.additionalRegisters)
  }

  private def modifyAsset(boxCandidate: ErgoBoxCandidate,
                          deltaFn: Long => Long,
                          idToskip: TokenId): ErgoBoxCandidate = {
    val assetId = boxCandidate.additionalTokens.find(t => !java.util.Arrays.equals(t._1, idToskip)).get._1

    val tokens = boxCandidate.additionalTokens.map { case (id, amount) =>
      if (java.util.Arrays.equals(id, assetId)) assetId -> deltaFn(amount) else assetId -> amount
    }

    new ErgoBoxCandidate(
      boxCandidate.value,
      boxCandidate.ergoTree,
      boxCandidate.creationHeight,
      tokens,
      boxCandidate.additionalRegisters)
  }

  private def checkTx(from: IndexedSeq[ErgoBox], wrongTx: ErgoTransaction): Try[Long] = {
    wrongTx.statelessValidity().flatMap(_ => wrongTx.statefulValidity(from, emptyDataBoxes, emptyStateContext))
  }

}
