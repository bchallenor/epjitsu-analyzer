package epjitsu

import epjitsu.util.PrettyPrint

sealed trait SaneSensorFlag
case object AdfOpen extends SaneSensorFlag
case object Hopper extends SaneSensorFlag
case object Top extends SaneSensorFlag
case object ScanSw extends SaneSensorFlag
case object Sleep extends SaneSensorFlag

object SaneSensorFlag {
  //todo: check what these mean and that they are true
  def unpack(flags: Set[Int]): Map[SaneSensorFlag, Boolean] = Map(
    AdfOpen -> flags(5),
    Hopper -> !flags(6),
    Top -> flags(7),
    ScanSw -> flags(8),
    Sleep -> flags(15)
  )

  implicit object SaneSensorFlagPrettyPrint extends PrettyPrint[SaneSensorFlag] {
    override def prettyPrint(value: SaneSensorFlag) = value.toString
  }
}
