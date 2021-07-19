package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.testkit.utils.NoShrink

class AutolykosPowSchemeSpec extends ErgoPropertyTest with NoShrink {

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    forAll(invalidHeaderGen,
            Gen.choose(80, 100),
            Gen.choose[Byte](1, 2)) { (inHeader, difficulty, ver) =>
      val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits, version = ver)
      val sk = randomSecret()
      val x = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      val hbs = Ints.toByteArray(h.height)
      val N = pow.calcN(h)
      val newHeader = pow.checkNonces(ver, hbs, msg, sk, x, b, N, 0, 1000)
        .map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success

      if(ver > Header.InitialVersion) {
        // We remove last byte of "msg", perform PoW and check that it fails validation
        require(HeaderSerializer.bytesWithoutPow(h).last == 0)
        val msg2 = Blake2b256(HeaderSerializer.bytesWithoutPow(h).dropRight(1))

        val newHeader2 = pow.checkNonces(ver, hbs, msg2, sk, x, b, N, 0, 1000)
          .map(s => h.copy(powSolution = s)).get
        pow.validate(newHeader2) shouldBe 'failure
      }
    }
  }

  property("calcN test vectors") {
    // mainnet parameters
    val k = 32
    val n = 26

    val pow = new AutolykosPowScheme(k, n)

    // N is always the same in Autolykos v1
    pow.calcN(1, 700000) shouldBe pow.NBase
    pow.calcN(1, 100000) shouldBe pow.NBase
    pow.calcN(1, 70000000) shouldBe pow.NBase

    pow.calcN(2, 500000) shouldBe pow.NBase
    pow.calcN(2, 600000) shouldBe pow.NBase
    pow.calcN(2, 600 * 1024) shouldBe 70464240
    pow.calcN(2, 650 * 1024) shouldBe 73987410
    pow.calcN(2, 700000) shouldBe 73987410
    pow.calcN(2, 788400) shouldBe 81571035 // 3 years
    pow.calcN(2, 1051200) shouldBe 104107290 // 4 years
    pow.calcN(2, 4198400) shouldBe 2143944600 // max height
    pow.calcN(2, 41984000) shouldBe 2143944600
  }

  // testing an invalid header got from a mining pool
  property("test vector - invalid solution") {
    import io.circe.parser._

    val headerJson = "{\"extensionId\":\"277907e4e5e42f27e928e6101cc4fec173bee5d7728794b73d7448c339c380e5\",\"difficulty\":\"1325481984\",\"votes\":\"000000\",\"timestamp\":1611225263165,\"size\":219,\"stateRoot\":\"c0d0b5eafd07b22487dac66628669c42a242b90bef3e1fcdc76d83140d58b6bc0e\",\"height\":2870,\"nBits\":72286528,\"version\":2,\"id\":\"5b0ce6711de6b926f60b67040cc4512804517785df375d063f1bf1d75588af3a\",\"adProofsRoot\":\"49453875a43035c7640dee2f905efe06128b00d41acd2c8df13691576d4fd85c\",\"transactionsRoot\":\"770cbb6e18673ed025d386487f15d3252115d9a6f6c9b947cf3d04731dd6ab75\",\"extensionHash\":\"9bc7d54583c5d44bb62a7be0473cd78d601822a626afc13b636f2cbff0d87faf\",\"powSolutions\":{\"pk\":\"0288114b0586efea9f86e4587f2071bc1c85fb77e15eba96b2769733e0daf57903\",\"w\":\"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798\",\"n\":\"000100000580a91b\",\"d\":0},\"adProofsId\":\"4fc36d59bf26a672e01fbfde1445bd66f50e0f540f24102e1e27d0be1a99dfbf\",\"transactionsId\":\"d196ef8a7ef582ab1fdab4ef807715183705301c6ae2ff0dcbe8f1d577ba081f\",\"parentId\":\"ab19e6c7a4062979dddb534df83f236d1b949c7cef18bcf434a67e87c593eef9\"}"

    val json = parse(headerJson).toOption.get

    val header = Header.jsonDecoder.decodeJson(json).toOption.get

    val pow = new AutolykosPowScheme(32, 26)

    pow.validate(header).isSuccess shouldBe false
  }

}
