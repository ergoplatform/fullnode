package org.ergoplatform.nodeView.wallet.scanning

/**
  * Values for a scan-related flag which shows how scan is interacting with the in-built p2pk wallet.
  * There are three options available:
  *  * off - if a box associated with a scan with interaction flag == off, the box will be added to the scan only
  *  * shared - box can be in both the wallet and the scan if wallet finds it (box is protected by wallet-related P2PK)
  *  * forced - box is always added to the wallet if it is added to the scan
  */
object ScanWalletInteraction extends Enumeration {

  type ScanWalletInteraction = Value

  val Off = Value("off")
  val Shared = Value("shared")
  val Forced = Value("forced")

  /**
    * Serializing a flag to a byte value representing it
    * @param v - flag
    * @return byte id (negative, this is used in scan deserialization)
    */
  def toByte(v: Value): Byte = v match {
    case Off => -1 : Byte
    case Shared => -2 : Byte
    case Forced => -3 : Byte
  }

  /**
    * Deserializing a flag from a byte value
    * @param b - byte value
    * @return flag deserialized
    */
  def fromByte(b: Byte): ScanWalletInteraction = b match {
    case x: Byte if x == -1 => Off
    case x: Byte if x == -2 => Shared
    case x: Byte if x == -3 => Forced
  }

  /**
    * Whether a box found for a scan with the flag provided should be added to a p2pk wallet
    * @param v - the flag
    * @return true if the box should be added to the p2pk wallet, false otherwise
    */
  def interactingWithWallet(v: Value): Boolean = v == Shared || v == Forced

}
