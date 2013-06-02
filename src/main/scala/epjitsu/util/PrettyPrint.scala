package epjitsu.util

import scala.collection.immutable.BitSet

trait PrettyPrint[-A] {
  def prettyPrint(value: A): String
}

object PrettyPrint {
  implicit object StringPrettyPrint extends PrettyPrint[String] {
    override def prettyPrint(value: String) = value
  }

  implicit object BooleanPrettyPrint extends PrettyPrint[Boolean] {
    override def prettyPrint(value: Boolean) = value.toString
  }

  implicit object BytePrettyPrint extends PrettyPrint[Byte] {
    override def prettyPrint(value: Byte) = f"0x$value%02x"
  }

  implicit object ShortPrettyPrint extends PrettyPrint[Short] {
    override def prettyPrint(value: Short) = f"0x$value%04x"
  }

  implicit object BytesPrettyPrint extends PrettyPrint[DeepByteArray] {
    val maxSize: Int = 128
    val maxRowSize: Int = 16
    override def prettyPrint(value: DeepByteArray) = {
      val underlying = value.underlying
      if (underlying.size <= maxSize) {
        val sb = new StringBuilder()
        sb.append("{")
        sb.append("\n")
        val rows = (underlying.iterator grouped maxRowSize withPartial true).toVector
        val lastRowIdx = rows.size - 1
        rows.zipWithIndex foreach { case (row, rowIdx) =>
          val isLast = rowIdx == lastRowIdx
          sb.append(row map ("0x%02x" format _) mkString ("", ", ", if (!isLast) "," else ""))
          sb.append("\n")
        }
        sb.append("}")
        sb.toString()
      } else {
        f"${underlying.size} (0x${underlying.size}%x) bytes"
      }
    }
  }

  implicit object BitSetPrettyPrint extends PrettyPrint[BitSet] {
    override def prettyPrint(value: BitSet) = value.toString()
  }

  implicit def TuplePrettyPrint[A, B](implicit ppa: PrettyPrint[A], ppb: PrettyPrint[B]) = new PrettyPrint[(A, B)] {
    override def prettyPrint(value: (A, B)) = value match { case (a, b) => s"${ppa.prettyPrint(a)} -> ${ppb.prettyPrint(b)}" }
  }

  implicit def MapPrettyPrint[A, B](implicit ppa: PrettyPrint[A], ppb: PrettyPrint[B]) = new PrettyPrint[Map[A, B]] {
    override def prettyPrint(value: Map[A, B]) = (value map { case (a, b) => (ppa.prettyPrint(a), ppb.prettyPrint(b)) }).toString()
  }
}
