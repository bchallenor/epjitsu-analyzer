package epjitsu

import org.joda.time.DateTime

trait Packet {
  def seqNo: Long
  def timestamp: DateTime
}

trait PacketDecoder[-A, +B <: Packet] {
  def decode(seqNo: Long, input: A): B
}
