package epjitsu

case class SaneTransferPhrase(hostPacket: UsbPacket, devicePacket: UsbPacket) extends PacketPhrase[UsbPacket] {
  override lazy val packets = List(hostPacket, devicePacket)

  require(hostPacket.packetType == UsbSubmit, s"Expected host packet to be a 'submit': $hostPacket")
  require(hostPacket.xferType == UsbBulk, s"Expected host packet to be a 'bulk' transfer: $hostPacket")
  require(devicePacket.packetType == UsbComplete, s"Expected device packet to be a 'complete': $devicePacket")
  require(devicePacket.xferType == UsbBulk, s"Expected device packet to be a 'bulk' transfer: $devicePacket")

  private val hostAddress: (Int, Int) = (hostPacket.bus, hostPacket.device)
  private val deviceAddress: (Int, Int) = (devicePacket.bus, devicePacket.device)
  require(hostAddress == deviceAddress, s"Expected host and device packets to target the same bus and device: $hostAddress vs $deviceAddress")

  val direction: UsbXferDir = {
    require(hostPacket.xferDir == devicePacket.xferDir, s"Expected host and device packets to have the same direction: ${hostPacket.xferDir} vs ${devicePacket.xferDir}")
    hostPacket.xferDir
  }

  val requestId: Long = {
    require(hostPacket.requestId == devicePacket.requestId, s"Expected host and device packets to have the same request ID: ${hostPacket.requestId} vs ${devicePacket.requestId}")
    hostPacket.requestId
  }

  val endpoint: Int = {
    require(hostPacket.endpoint == devicePacket.endpoint, s"Expected host and device packets to have the same endpoint: ${hostPacket.endpoint} vs ${devicePacket.endpoint}")
    hostPacket.endpoint
  }

  val bytes: Array[Byte] = direction match {
    case UsbIn =>
      require(hostPacket.bytes.isEmpty, s"Expected empty host bytes for 'in' transfer: ${hostPacket.bytes}")
      devicePacket.bytes
    case UsbOut =>
      require(devicePacket.bytes.isEmpty, s"Expected empty device bytes for 'out' transfer: ${devicePacket.bytes}")
      hostPacket.bytes
  }

  private lazy val directionStr = direction match {
    case UsbOut => "-->"
    case UsbIn => "<--"
  }

  override def toString: String = f"${hostPacket.seqNo} $directionStr ${formatBytes(bytes)}"
}

object SaneTransferPhraseDecoder extends PacketPhraseDecoder[UsbPacket, SaneTransferPhrase] {
  def decode(bulkUsbPackets: Stream[UsbPacket]): Stream[SaneTransferPhrase] = {
    // Pair off packets after sorting by (requestId, seqNo)
    (bulkUsbPackets sortBy (x => (x.requestId, x.seqNo)) grouped 2).toStream map ( _ match {
      case Stream(host, device) => SaneTransferPhrase(host, device)
      case other => sys.error(s"Expected an even number of USB packets")
    })
  }
}
