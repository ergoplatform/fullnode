package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.scanning.ScanWalletInteraction.ScanWalletInteraction
import org.ergoplatform.wallet.Constants.ScanId
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.{Failure, Success, Try}



/**
  * Wraps information about user scan.
  *
  * A scan is providing scanning rules for specific boxes, and then work with boxes found by the node.
  *
  * @param scanId         - unique identifier of an scan in the local system
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  * @param walletInteraction - a flag which is prescribing how the scan is interacting with the p2pk-wallet
  */
case class Scan(scanId: ScanId,
                scanName: String,
                trackingRule: ScanningPredicate,
                walletInteraction: ScanWalletInteraction.Value,
                removeOffchain: Boolean)

object Scan {

  val MaxScanNameLength = 255

}

object ScanSerializer extends ScorexSerializer[Scan] {

  private val trueNeg: Byte = -1
  private val falseNeg: Byte = -2

  override def serialize(app: Scan, w: Writer): Unit = {
    w.putShort(app.scanId)
    w.putShortString(app.scanName)
    w.put(ScanWalletInteraction.toByte(app.walletInteraction))
    if (app.removeOffchain) {
      w.put(trueNeg)
    } else {
      w.put(falseNeg)
    }
    ScanningPredicateSerializer.serialize(app.trackingRule, w)
  }

  override def parse(r: Reader): Scan = {
    val scanId = ScanId @@ r.getShort()
    val appName = r.getShortString()

    // hack to read scans serialized with previous versions (they will have positive first byte)
    // for scans written with previous versions, walletInteraction flag is set to "off"
    val (interactionFlag, removeOffchain) = r.peekByte() match {
      case x: Byte if x < 0 => {
        r.getByte()
        val wi = ScanWalletInteraction.fromByte(x)
        r.peekByte() match {
          case x: Byte if x < 0 =>
            val b = r.getByte()
            val ro = if (b == trueNeg) true else false
            (wi, ro)
          case _ => (wi, true) // default value for removeOffchain
        }
      }
      case _ => (ScanWalletInteraction.Off, true) // default values
    }
    val sp = ScanningPredicateSerializer.parse(r)
    Scan(scanId, appName, sp, interactionFlag, removeOffchain)
  }

}

/**
  * A class that encodes an API request to create a scan.
  *
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  * @param walletInteraction - how scan should interact with (p2pk) wallet, see @ScanWalletInteraction for details
  *
  */
case class ScanRequest(scanName: String,
                       trackingRule: ScanningPredicate,
                       walletInteraction: Option[ScanWalletInteraction],
                       removeOffchain: Option[Boolean]) {
  def toScan(scanId: ScanId): Try[Scan] = {
    if (scanName.getBytes("UTF-8").length > Scan.MaxScanNameLength) {
      Failure(new Exception(s"Too long scan name: $scanName"))
    } else {
      val wi = walletInteraction.getOrElse(ScanWalletInteraction.Shared)
      val ro = removeOffchain.getOrElse(true)
      val scan = Scan(scanId, scanName, trackingRule, wi, ro)
      Success(scan)
    }
  }
}

object ScanJsonCodecs extends ApiCodecs {

  import ScanningPredicateJsonCodecs._
  import io.circe._, io.circe.generic.semiauto._

  implicit val interactionFlagDecoder: Decoder[ScanWalletInteraction.Value] = Decoder.decodeEnumeration(ScanWalletInteraction)
  implicit val interactionFlagEncoder: Encoder[ScanWalletInteraction.Value] = Encoder.encodeEnumeration(ScanWalletInteraction)

  implicit val scanReqDecoder: Decoder[ScanRequest] = deriveDecoder[ScanRequest]
  implicit val scanReqEncoder: Encoder[ScanRequest] = deriveEncoder[ScanRequest]

  implicit val scanDecoder: Decoder[Scan] = deriveDecoder[Scan]
  implicit val scanEncoder: Encoder[Scan] = deriveEncoder[Scan]
}
