package org.ergoplatform.nodeView.wallet.scanning

import io.circe.Json
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.WalletGenerators

class ScanSpecification extends ErgoPropertyTest with WalletGenerators {
  import ScanJsonCodecs._

  property("external scan req json serialization roundtrip") {
    forAll(externalScanReqGen) { req =>
      val j: Json = scanReqEncoder.apply(req)
      scanReqDecoder.decodeJson(j).toTry.get == req
    }
  }

  property("external scan json serialization roundtrip") {
    forAll(externalAppGen) { req =>
      val j: Json = scanEncoder.apply(req)
      scanDecoder.decodeJson(j).toTry.get == req
    }
  }

}
