package epjitsu

import org.joda.time.DateTime

sealed trait SanePacket extends Packet {
  def direction: UsbXferDir
  def requestId: Long
  def endpoint: Int
  def bytes: Array[Byte]

  private def directionStr = direction match {
    case UsbOut => "-->"
    case UsbIn => "<--"
  }

  private def bytesStr = if (bytes.size <= 72) bytes map ("0x%02x" format _) mkString("{", ", ", "}") else s"${bytes.size} bytes"

  override def toString: String = f"0x$requestId%016x 0x$endpoint%02x $directionStr $bytesStr"
}

case class SaneSend(timestamp: DateTime, requestId: Long, endpoint: Int, bytes: Array[Byte]) extends SanePacket {
  override def direction = UsbOut
}

case class SaneReceive(timestamp: DateTime, requestId: Long, endpoint: Int, bytes: Array[Byte]) extends SanePacket {
  override def direction = UsbIn
}

object SanePacketDecoder extends PacketDecoder[(UsbPacket, UsbPacket), SanePacket] {
  def decode(input: (UsbPacket, UsbPacket)): SanePacket = {
    input match {
      case (x, y) =>
        val xAddress = (x.bus, x.device)
        val yAddress = (y.bus, y.device)
        require(xAddress == yAddress, s"Packets must be from the same bus and device: $xAddress vs $yAddress")
    }

    input match {
      case (
        UsbPacket(spTs, spId, UsbSubmit, UsbBulk, UsbOut, _, _, spEndpoint, spBytes),
        UsbPacket(_, cpId, UsbComplete, UsbBulk, UsbOut, _, _, cpEndpoint, Array())
      ) if spId == cpId && spEndpoint == cpEndpoint => SaneSend(spTs, spId, spEndpoint, spBytes)

      case (
        UsbPacket(_, spId, UsbSubmit, UsbBulk, UsbIn, _, _, spEndpoint, Array()),
        UsbPacket(cpTs, cpId, UsbComplete, UsbBulk, UsbIn, _, _, cpEndpoint, cpBytes)
      ) if spId == cpId && spEndpoint == cpEndpoint => SaneReceive(cpTs, cpId, cpEndpoint, cpBytes)

      case (x, y) => sys.error(f"Could not match ${x.requestId}%16x with ${y.requestId}%16x to make a SanePacket: $x, $y")
    }
  }
}
