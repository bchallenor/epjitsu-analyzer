package epjitsu

import org.joda.time.DateTime

trait PcapPacket extends Packet {
  def timestamp: DateTime
}
