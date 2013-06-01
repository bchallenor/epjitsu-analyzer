package epjitsu

import org.scalacheck.{Arbitrary, Properties}
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties
import scala.collection.immutable.SortedSet

case class NullPacket(seqNo: Long) extends Packet

object PacketPhraseSpecification extends Properties("PacketPhrase") {
  implicit lazy val arbNullPacket: Arbitrary[NullPacket] =
    Arbitrary(
      for {
        seqNo <- Arbitrary.arbitrary[Long]
      } yield NullPacket(seqNo)
    )

  implicit def arbPacketPhrase[P <: Packet: Arbitrary, A: Arbitrary]: Arbitrary[PacketPhrase[P, A]] =
    Arbitrary(
      for {
        packets <- Arbitrary.arbitrary[Set[P]]
        value <- Arbitrary.arbitrary[A]
      } yield PacketPhrase(packets.to[SortedSet], value)
    )

  type NullPacketPhrase[A] = PacketPhrase[NullPacket, A]
  include(ScalazProperties.equal.laws[PacketPhrase[NullPacket, Int]])
  include(ScalazProperties.monoid.laws[PacketPhrase[NullPacket, Int]])
  include(ScalazProperties.applicative.laws[NullPacketPhrase])
  include(ScalazProperties.monad.laws[NullPacketPhrase])
}
