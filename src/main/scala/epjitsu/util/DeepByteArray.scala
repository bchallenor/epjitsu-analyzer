package epjitsu.util

import java.util

// todo: handle other primitive types
sealed case class DeepByteArray(underlying: Array[Byte]) extends Equals {
  override def toString: String = underlying.toString

  override lazy val hashCode = util.Arrays.hashCode(this.underlying)

  override def equals(other: Any): Boolean = other match {
    case that: DeepByteArray => util.Arrays.equals(this.underlying, that.underlying)
    case _ => false
  }

  def canEqual(other : Any): Boolean = other.isInstanceOf[DeepByteArray]
}

object DeepByteArray {
  implicit def make(underlying: Array[Byte]) = DeepByteArray(underlying)
}
