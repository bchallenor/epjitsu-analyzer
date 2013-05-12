package epjitsu

import org.joda.time.DateTime

trait Packet {
  def seqNo: Long
  def timestamp: DateTime

  protected def formatBytes(bytes: Array[Byte]) = if (bytes.size <= 128) bytes map ("0x%02x" format _) mkString("{", ", ", "}") else s"${bytes.size} bytes"
}

trait PacketDecoder[-I, +P <: Packet] {
  def decode(seqNo: Long, input: I): P
}

trait PacketPhrase[+P <: Packet] extends Packet {
  def packets: List[P]

  lazy val head: P = packets minBy (_.seqNo)
  override lazy val seqNo = head.seqNo
  override lazy val timestamp = head.timestamp
}

trait PacketPhraseDecoder[P <: Packet, +Q <: PacketPhrase[P]] {
  def decode(inputPackets: Stream[P]): Stream[Q]
}
