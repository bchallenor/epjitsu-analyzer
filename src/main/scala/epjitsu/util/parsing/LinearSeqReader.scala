package epjitsu.util.parsing

import scala.util.parsing.input.{Reader, Position}
import scala.collection.immutable.LinearSeq

class LinearSeqReader[+A](seq: LinearSeq[A], val index: Int = 0) extends Reader[A] {
  override lazy val first: A = seq.head
  override lazy val rest: Reader[A] = new LinearSeqReader(seq.tail, index + 1)
  override lazy val pos: Position = new LinearSeqPosition[A](seq, index)
  override lazy val atEnd: Boolean = seq.isEmpty
}
