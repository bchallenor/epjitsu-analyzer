package epjitsu

import java.io.DataInput

case class UsbPayload(requestId: Long, packetType: UsbPacketType, xferType: UsbXferType, xferDir: UsbXferDir, bus: Int, device: Int, endpoint: Int, bytes: Array[Byte]) extends Payload

sealed trait UsbPacketType
case object UsbSubmit extends UsbPacketType
case object UsbComplete extends UsbPacketType

sealed trait UsbXferType
case object UsbIsochronous extends UsbXferType
case object UsbInterrupt extends UsbXferType
case object UsbControl extends UsbXferType
case object UsbBulk extends UsbXferType

sealed trait UsbXferDir
case object UsbIn extends UsbXferDir
case object UsbOut extends UsbXferDir

object LinuxUsbPayloadDecoder extends PayloadDecoder {
  override def decode(dataInput: DataInput): UsbPayload = {
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

    val bytes = Array.ofDim[Byte](len_cap)
    dataInput.readFully(bytes)

    UsbPayload(id, packetType, xferType, xferDir, busnum, devnum, endpoint, bytes)
  }

  private def decodePacketType(packetType: Byte): UsbPacketType = {
    packetType match {
      case 'S' => UsbSubmit
      case 'C' => UsbComplete
      case _ => sys.error(s"Unknown packet type $packetType")
    }
  }

  private def decodeXferType(xferType: Byte): UsbXferType = {
    xferType match {
      case 0 => UsbIsochronous
      case 1 => UsbInterrupt
      case 2 => UsbControl
      case 3 => UsbBulk
      case _ => sys.error(s"Unknown transfer type $xferType")
    }
  }

  private def decodeEndpointAndXferDir(epnum: Byte): (Int, UsbXferDir) = {
    val endpoint = epnum & 0x7f
    val xferDir = if ((epnum & 0x80) == 0) UsbOut else UsbIn
    (endpoint, xferDir)
  }
}
