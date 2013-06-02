package epjitsu.util

import scala.math.Ordering
import scalaz.Monoid
import scala.collection.immutable.SortedSet

object XInstances {
  implicit def SortedSetMonoid[A: Ordering]: Monoid[SortedSet[A]] = new Monoid[SortedSet[A]] {
    override def zero: SortedSet[A] = SortedSet.empty[A]
    override def append(f1: SortedSet[A], f2: => SortedSet[A]): SortedSet[A] = f1 union f2
  }
}
