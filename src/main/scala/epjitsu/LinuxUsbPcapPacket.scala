package epjitsu

import org.joda.time.DateTime
import epjitsu.util.PrettyPrint.BytesPrettyPrint
import epjitsu.util.DeepByteArray
import java.io.DataInput

// Linux packets have "submits" and "completes" that need matching; a pair corresponds to a transfer

case class LinuxUsbPcapPacket(seqNo: Long, timestamp: DateTime, requestId: Long, packetType: UsbPacketType, xferType: UsbTransferType, direction: TransferDir, bus: Int, device: Int, endpoint: Int, bytes: Array[Byte]) extends PcapPacket {
  override def toString: String = s"LinuxUsbPcapPacket($seqNo, $timestamp, $requestId, $packetType, $xferType, $direction, $bus, $device, $endpoint, ${BytesPrettyPrint.prettyPrint(DeepByteArray.make(bytes))})"
}

object LinuxUsbPcapPacketDecoder extends PacketDecoder[(DateTime, DataInput), LinuxUsbPcapPacket] {
  override def decode(seqNo: Long, context: (DateTime, DataInput)): LinuxUsbPcapPacket = {
    val (timestamp, dataInput) = context
    val id = dataInput.readLong()
    val packet_type = dataInput.readByte()
    val xfer_type = dataInput.readByte()
    val epnum = dataInput.readByte()
    val devnum = dataInput.readByte()
    val busnum = dataInput.readUnsignedShort()
    val flag_setup = dataInput.readByte()
    val flag_data = dataInput.readByte()
    val ts_sec = dataInput.readLong()
    val ts_usec = dataInput.readInt()
    val status = dataInput.readInt()
    val length = dataInput.readInt()
    val len_cap = dataInput.readInt()
    val s = dataInput.readLong()
    val interval = dataInput.readInt()
    val start_frame = dataInput.readInt()
    val xfer_flags = dataInput.readInt()
    val ndesc = dataInput.readInt()

    val packetType = decodePacketType(packet_type)
    val xferType = decodeXferType(xfer_type)
    val (endpoint, xferDir) = decodeEndpointAndXferDir(epnum)

    //Just use the one from the header
    //val timestamp = new DateTime(ts_sec * 1000L + ts_usec / 1000L)

    val bytes = Array.ofDim[Byte](len_cap)
    dataInput.readFully(bytes)

    LinuxUsbPcapPacket(seqNo, timestamp, id, packetType, xferType, xferDir, busnum, devnum, endpoint, bytes)
  }

  private def decodePacketType(packetType: Byte): UsbPacketType = {
    packetType match {
      case 'S' => UsbSubmit
      case 'C' => UsbComplete
      case _ => sys.error(s"Unknown packet type $packetType")
    }
  }

  private def decodeXferType(xferType: Byte): UsbTransferType = {
    xferType match {
      case 0 => UsbIsochronous
      case 1 => UsbInterrupt
      case 2 => UsbControl
      case 3 => UsbBulk
      case _ => sys.error(s"Unknown transfer type $xferType")
    }
  }

  private def decodeEndpointAndXferDir(epnum: Byte): (Int, TransferDir) = {
    val endpoint = epnum & 0x7f
    val xferDir = if ((epnum & 0x80) == 0) OutDir else InDir
    (endpoint, xferDir)
  }
}

object LinuxUsbPcapPacketTranslator extends PacketStreamTranslator[LinuxUsbPcapPacket, UsbTransfer] {
  override def translate(inputPackets: Stream[LinuxUsbPcapPacket]): Stream[UsbTransfer] = {
    val bulkTransfers = inputPackets filter (_.xferType == UsbBulk) // todo: handle other types

    // Pair off packets after sorting by (requestId, seqNo)
    val transfers = (bulkTransfers sortBy (x => (x.requestId, x.seqNo)) grouped 2).toStream map ( _ match {
      case Stream(host, device) => buildTransfer(host, device)
      case other => sys.error(s"Expected an even number of USB packets")
    })

    // Put back in original order
    transfers sortBy (_.seqNo)
  }

  private def buildTransfer(hostPacket: LinuxUsbPcapPacket, devicePacket: LinuxUsbPcapPacket): UsbTransfer = {
    require(hostPacket.packetType == UsbSubmit, s"Expected host packet to be a 'submit': $hostPacket")
    require(devicePacket.packetType == UsbComplete, s"Expected device packet to be a 'complete': $devicePacket")

    val seqNo: Long = hostPacket.seqNo
    val timestamp: DateTime = hostPacket.timestamp

    val requestId: Long = {
      require(hostPacket.requestId == devicePacket.requestId, s"Expected host and device packets to have the same request ID: ${hostPacket.requestId} vs ${devicePacket.requestId}")
      hostPacket.requestId
    }

    val xferType: UsbTransferType = {
      require(hostPacket.xferType == devicePacket.xferType, s"Expected host and device packets to have the same xferType: ${hostPacket.xferType} vs ${devicePacket.xferType}")
      hostPacket.xferType
    }

    val direction: TransferDir = {
      require(hostPacket.direction == devicePacket.direction, s"Expected host and device packets to have the same direction: ${hostPacket.direction} vs ${devicePacket.direction}")
      hostPacket.direction
    }

    val bus: Int = {
      require(hostPacket.bus == devicePacket.bus, s"Expected host and device packets to have the same bus: ${hostPacket.bus} vs ${devicePacket.bus}")
      hostPacket.bus
    }

    val device: Int = {
      require(hostPacket.device == devicePacket.device, s"Expected host and device packets to have the same device: ${hostPacket.device} vs ${devicePacket.device}")
      hostPacket.device
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

    UsbTransfer(seqNo, timestamp, requestId, xferType, direction, bus, device, endpoint, bytes)
  }
}
