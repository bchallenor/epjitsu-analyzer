package epjitsu

import org.joda.time.DateTime

sealed trait SanePacket extends Packet {
  def direction: UsbXferDir
  def requestId: Long
  def bytes: Array[Byte]

  private def directionStr = direction match {
    case UsbOut => "-->"
    case UsbIn => "<--"
  }

  private def bytesStr = if (bytes.size <= 72) bytes map ("0x%02x" format _) mkString("{", ", ", "}") else s"${bytes.size} bytes"

  override def toString: String = f"0x$requestId%16x $directionStr $bytesStr"
}

case class SaneSend(timestamp: DateTime, requestId: Long, bytes: Array[Byte]) extends SanePacket {
  override def direction = UsbOut
}

case class SaneReceive(timestamp: DateTime, requestId: Long, bytes: Array[Byte]) extends SanePacket {
  override def direction = UsbIn
}

object SanePacketDecoder extends PacketDecoder[(UsbPacket, UsbPacket), SanePacket] {
  def decode(input: (UsbPacket, UsbPacket)): SanePacket = {
    input match {
      case (
        UsbPacket(spTs, spId, UsbSubmit, UsbBulk, UsbOut, _, _, _, spBytes),
        UsbPacket(_, cpId, UsbComplete, UsbBulk, UsbOut, _, _, _, Array())
      ) if spId == cpId => SaneSend(spTs, spId, spBytes)

      case (
        UsbPacket(_, spId, UsbSubmit, UsbBulk, UsbIn, _, _, _, Array()),
        UsbPacket(cpTs, cpId, UsbComplete, UsbBulk, UsbIn, _, _, _, cpBytes)
      ) if spId == cpId => SaneReceive(cpTs, cpId, cpBytes)

      case (x, y) => sys.error(f"Could not match ${x.requestId}%16x with ${y.requestId}%16x to make a SanePacket: $x, $y")
    }
  }
}
