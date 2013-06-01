package epjitsu

import scalaz._
import Scalaz._
import scala.math.Ordering
import scala.collection.immutable.SortedSet

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

case class PacketPhrase[P <: Packet, +A](packets: SortedSet[P], value: A) {
  def map[B](f: A => B): PacketPhrase[P, B] = PacketPhrase(packets, f(value))

  def flatMap[B](f: A => PacketPhrase[P, B]): PacketPhrase[P, B] = {
    val pp = f(value)
    PacketPhrase(packets union pp.packets, pp.value)
  }
}

object PacketPhrase {
  implicit def packetPhraseEqual[P <: Packet, A]: Equal[PacketPhrase[P, A]] = new Equal[PacketPhrase[P, A]] {
    override def equal(a1: PacketPhrase[P, A], a2: PacketPhrase[P, A]): Boolean = a1 == a2
  }

  implicit def packetPhraseMonoid[P <: Packet, A: Monoid]: Monoid[PacketPhrase[P, A]] = new Monoid[PacketPhrase[P, A]] {
    override def zero: PacketPhrase[P, A] = PacketPhrase(SortedSet.empty[P], implicitly[Monoid[A]].zero)
    override def append(f1: PacketPhrase[P, A], f2: => PacketPhrase[P, A]): PacketPhrase[P, A] = PacketPhrase(f1.packets union f2.packets, f1.value |+| f2.value)
  }

  implicit def packetPhraseApplicative[P <: Packet]: Applicative[({type M[A] = PacketPhrase[P, A]})#M] = new Applicative[({type M[A] = PacketPhrase[P, A]})#M] {
    override def point[A](a: => A): PacketPhrase[P, A] = PacketPhrase(SortedSet.empty[P], a)
    override def ap[A,B](fa: => PacketPhrase[P, A])(f: => PacketPhrase[P, A => B]): PacketPhrase[P, B] = PacketPhrase(f.packets union fa.packets, f.value(fa.value))
  }

  implicit def packetPhraseBind[P <: Packet]: Bind[({type M[A] = PacketPhrase[P, A]})#M] = new Bind[({type M[A] = PacketPhrase[P, A]})#M] {
    override def map[A, B](r: PacketPhrase[P, A])(f: A => B) = r map f
    override def bind[A, B](r: PacketPhrase[P, A])(f: A => PacketPhrase[P, B]): PacketPhrase[P, B] =  r flatMap f
  }

  implicit def packetPhraseMonad[P <: Packet]: Monad[({type M[A] = PacketPhrase[P, A]})#M] = new Monad[({type M[A] = PacketPhrase[P, A]})#M] {
    override def map[A, B](r: PacketPhrase[P, A])(f: A => B) = r map f
    override def point[A](a: => A): PacketPhrase[P, A] = PacketPhrase[P, A](SortedSet.empty[P], a)
    override def bind[A, B](r: PacketPhrase[P, A])(f: A => PacketPhrase[P, B]): PacketPhrase[P, B] =  r flatMap f
  }
}

trait PacketPhraseDecoder[P <: Packet, +A] {
  def decode(inputPackets: Stream[P]): Stream[PacketPhrase[P, A]]
}
