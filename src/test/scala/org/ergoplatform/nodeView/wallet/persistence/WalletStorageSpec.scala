package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage.SecretPathsKey
import org.ergoplatform.nodeView.wallet.scanning.{ScanRequest, ScanWalletInteraction}
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.secrets.{DerivationPathSerializer, DerivationPath}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.db.LDBKVStore
import scorex.testkit.utils.FileUtils

class WalletStorageSpec
  extends AnyFlatSpec
    with Matchers
    with WalletGenerators
    with ScalaCheckPropertyChecks
    with DBSpec
    with FileUtils {

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  it should "add and read derivation paths" in {
    def addPath(store: LDBKVStore, storedPaths: Seq[DerivationPath], derivationPath: DerivationPath): Unit = {
      val updatedPaths = (storedPaths :+ derivationPath).toSet
      val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
        .foldLeft(Array.empty[Byte]) { case (acc, path) =>
          val bytes = DerivationPathSerializer.toBytes(path)
          acc ++ Ints.toByteArray(bytes.length) ++ bytes
        }
      store.insert(Seq(SecretPathsKey -> toInsert))
    }

    forAll(Gen.nonEmptyListOf(derivationPathGen)) { paths =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        paths.foreach(path => addPath(store, storage.readPaths(), path))
        storage.readPaths() should contain theSameElementsAs paths.toSet
      }
    }
  }

  it should "add and read public keys" in {
    forAll(extendedPubKeyListGen) { pubKeys =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        pubKeys.foreach(storage.addKey)
        val keysRead = storage.readAllKeys()
        keysRead.length shouldBe pubKeys.length
        keysRead should contain theSameElementsAs pubKeys.toSet
      }
    }
  }

  it should "add, remove and read scans" in {
    forAll(Gen.nonEmptyListOf(externalScanReqGen)) { externalScanReqs =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        externalScanReqs.foreach(req => storage.addScan(req))
        val storageApps = storage.allScans
        val storageRequests = storageApps.map { app =>
          ScanRequest(app.scanName, app.trackingRule, Some(ScanWalletInteraction.Off), Some(false))
        }
        storageRequests.foreach(r => externalScanReqs.contains(r) shouldBe true)
        storageApps.map(_.scanId).foreach(storage.removeScan)
        storage.allScans.length shouldBe 0
      }
    }
  }

  it should "always increase ids" in {
    forAll(externalScanReqGen) { externalScanReq =>
      withStore { store =>
        val storage = new WalletStorage(store, settings)
        val scan = storage.addScan(externalScanReq).get

        storage.lastUsedScanId shouldBe scan.scanId

        storage.removeScan(scan.scanId)
        storage.lastUsedScanId shouldBe scan.scanId

        val scan2 = storage.addScan(externalScanReq).get
        storage.lastUsedScanId shouldBe scan2.scanId
        storage.lastUsedScanId shouldBe (scan.scanId +1)
      }
    }
  }

}
