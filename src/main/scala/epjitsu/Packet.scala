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

trait PacketStreamDecoder[-I, +P <: Packet] {
  def decode(input: I): Stream[P]
}

trait PacketStreamTranslator[-P <: Packet, +Q <: Packet] {
  def translate(inputPackets: Stream[P]): Stream[Q]
}

object PacketStreamTranslator {
  def identity[P <: Packet] = new PacketStreamTranslator[P, P] {
    override def translate(inputPackets: Stream[P]): Stream[P] = inputPackets
  }
}
