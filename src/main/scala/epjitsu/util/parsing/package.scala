package epjitsu.util

import scala.collection.immutable.LinearSeq

package object parsing {
  implicit def listToLinearSeqReader[A](seq: LinearSeq[A]) = new LinearSeqReader[A](seq)
}
