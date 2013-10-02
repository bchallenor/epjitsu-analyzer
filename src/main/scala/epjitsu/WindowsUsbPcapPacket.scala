package epjitsu

import org.joda.time.DateTime
import epjitsu.util.PrettyPrint.BytesPrettyPrint
import epjitsu.util.DeepByteArray
import java.io.DataInput

// Windows packets correspond 1:1 to a transfer

case class WindowsUsbPcapPacket(seqNo: Long, timestamp: DateTime, requestId: Long, xferType: UsbTransferType, direction: TransferDir, bus: Int, device: Int, endpoint: Int, bytes: Array[Byte]) extends PcapPacket {
  override def toString: String = s"WindowsUsbPcapPacket($seqNo, $requestId, $xferType, $direction, $bus, $device, $endpoint, ${BytesPrettyPrint.prettyPrint(DeepByteArray.make(bytes))})"
}

object WindowsUsbPcapPacketDecoder extends PacketDecoder[(DateTime, DataInput), WindowsUsbPcapPacket] {
  override def decode(seqNo: Long, context: (DateTime, DataInput)): WindowsUsbPcapPacket = {
    val (timestamp, dataInput) = context
    val headerLen = dataInput.readUnsignedShort()/* This header length */
    val irpId = dataInput.readLong()/* I/O Request packet ID */
    val status = dataInput.readInt()/* USB status code (on return from host controller) */
    val function = dataInput.readUnsignedShort()/* URB Function */
    val info = dataInput.readUnsignedByte()/* I/O Request info */
    val bus = dataInput.readUnsignedShort()/* bus (RootHub) number */
    val device = dataInput.readUnsignedShort()/* device address */
    val endpointAndXferDir = dataInput.readByte()/* endpoint number and transfer direction */
    val transfer = dataInput.readByte()/* transfer type */

    val dataLength = dataInput.readInt()/* Data length */

    dataInput.skipBytes(headerLen - 27) // we don't interpret the transfer-specific headers

    val xferType = decodeXferType(transfer)
    val (endpoint, xferDir) = decodeEndpointAndXferDir(endpointAndXferDir)

    val bytes = Array.ofDim[Byte](dataLength)
    dataInput.readFully(bytes)

    WindowsUsbPcapPacket(seqNo, timestamp, irpId, xferType, xferDir, bus, device, endpoint, bytes)
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

object WindowsUsbPcapPacketTranslator extends PacketStreamTranslator[WindowsUsbPcapPacket, UsbTransfer] {
  override def translate(inputPackets: Stream[WindowsUsbPcapPacket]): Stream[UsbTransfer] = {
    inputPackets map buildTransfer
  }

  private def buildTransfer(packet: WindowsUsbPcapPacket): UsbTransfer = {
    import packet._
    UsbTransfer(seqNo, timestamp, requestId, xferType, direction, bus, device, endpoint, bytes)
  }
}
