package epjitsu

import org.joda.time.DateTime

trait Packet {
  def timestamp: DateTime
}

trait PacketDecoder[-A, +B <: Packet] {
  def decode(input: A): B
}
