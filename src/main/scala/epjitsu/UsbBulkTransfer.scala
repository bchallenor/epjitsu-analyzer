package epjitsu

import epjitsu.util.PrettyPrint.BytesPrettyPrint

case class UsbBulkTransfer(hostPacket: UsbPacket, devicePacket: UsbPacket) extends Transfer with Packet {
  require(hostPacket.packetType == UsbSubmit, s"Expected host packet to be a 'submit': $hostPacket")
  require(hostPacket.xferType == UsbBulk, s"Expected host packet to be a 'bulk' transfer: $hostPacket")
  require(devicePacket.packetType == UsbComplete, s"Expected device packet to be a 'complete': $devicePacket")
  require(devicePacket.xferType == UsbBulk, s"Expected device packet to be a 'bulk' transfer: $devicePacket")

  override val seqNo: Long = hostPacket.seqNo

  private val hostAddress: (Int, Int) = (hostPacket.bus, hostPacket.device)
  private val deviceAddress: (Int, Int) = (devicePacket.bus, devicePacket.device)
  require(hostAddress == deviceAddress, s"Expected host and device packets to target the same bus and device: $hostAddress vs $deviceAddress")

  val direction: TransferDir = {
    require(hostPacket.dir == devicePacket.dir, s"Expected host and device packets to have the same direction: ${hostPacket.dir} vs ${devicePacket.dir}")
    hostPacket.dir
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
    case InDir =>
      require(hostPacket.bytes.isEmpty, s"Expected empty host bytes for 'in' transfer: ${hostPacket.bytes}")
      devicePacket.bytes
    case OutDir =>
      require(devicePacket.bytes.isEmpty, s"Expected empty device bytes for 'out' transfer: ${devicePacket.bytes}")
      hostPacket.bytes
  }

  private lazy val directionStr = direction match {
    case OutDir => "-->"
    case InDir => "<--"
  }

  override def toString: String = f"#${hostPacket.seqNo} $hostAddress $directionStr $deviceAddress ${BytesPrettyPrint.prettyPrint(bytes)}"
}

object UsbBulkTransfer {
  type UsbBulkTransferPhrase[A] = PacketPhrase[UsbBulkTransfer, A]
}

object UsbBulkTransferDecoder extends PacketStreamDecoder[UsbPacket, UsbBulkTransfer] {
  override def decode(bulkUsbPackets: Stream[UsbPacket]): Stream[UsbBulkTransfer] = {
    // Pair off packets after sorting by (requestId, seqNo)
    val transfers = (bulkUsbPackets sortBy (x => (x.requestId, x.seqNo)) grouped 2).toStream map ( _ match {
      case Stream(host, device) => UsbBulkTransfer(host, device)
      case other => sys.error(s"Expected an even number of USB packets")
    })
    // Put back in original order
    transfers sortBy (_.seqNo)
  }
}
