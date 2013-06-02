package epjitsu

import scalaz._
import Scalaz._
import scala.math.Ordering

trait Packet {
  def seqNo: Long
}

object Packet {
  implicit def seqNoOrdering[P <: Packet] = new Ordering[P] {
    private val optionOrdering = implicitly[Ordering[Long]]
    def compare(x: P, y: P): Int = optionOrdering.compare(x.seqNo, y.seqNo)
  }
}

trait PacketDecoder[-I, +P <: Packet] {
  def decode(seqNo: Long, input: I): P
}

trait PacketStreamDecoder[P <: Packet, Q <: Packet] {
  def decode(inputPackets: Stream[P]): Stream[Q]
}
