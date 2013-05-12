package epjitsu

import org.joda.time.DateTime
import java.io.DataInput
import scalaz._
import Scalaz._

case class PcapPacket(timestamp: DateTime, innerPacket: Packet) extends Packet

class PcapPacketDecoder(networkType: Int) extends PacketDecoder[DataInput, PcapPacket] {
  private val innerDecoder = PcapPacketDecoder.networkTypeToDecoder.get(networkType) err s"No decoder for network type $networkType"

  def decode(dataInput: DataInput): PcapPacket = {
    val timestampSecs = dataInput.readInt()
    val timestampMicros = dataInput.readInt()
    val timestamp = new DateTime(timestampSecs * 1000L + timestampMicros / 1000L)

    val includedLength = dataInput.readInt()
    val originalLength = dataInput.readInt()
    if (includedLength != originalLength) sys.error(s"Missing packet data: capture includes only $includedLength bytes of $originalLength")

    val innerPacket = innerDecoder.decode(dataInput)

    PcapPacket(timestamp, innerPacket)
  }
}

object PcapPacketDecoder {
  private val networkTypeToDecoder: Map[Int, PacketDecoder[DataInput, Packet]] = Map(
    220 -> LinuxUsbPacketDecoder
  )
}
