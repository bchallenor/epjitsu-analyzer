package epjitsu

import java.io.DataInput
import org.joda.time.DateTime

trait Packet {
  def timestamp: DateTime
}

trait PacketDecoder[+A <: Packet] {
  def decode(dataInput: DataInput): A
}
