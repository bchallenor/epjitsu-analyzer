package epjitsu.util.parsing

import scala.util.parsing.input.Position
import scala.collection.immutable.LinearSeq

class LinearSeqPosition[+A](seq: LinearSeq[A], index: Int) extends Position {
  def line: Int = index + 1
  def column: Int = 1
  protected def lineContents: String = if (seq.isEmpty) "" else seq.head.toString
}
