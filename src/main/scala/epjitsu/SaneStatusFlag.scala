package epjitsu

import epjitsu.util.PrettyPrint

sealed trait SaneStatusFlag
case object UsbPower extends SaneStatusFlag
case object FirmwareLoaded extends SaneStatusFlag

object SaneStatusFlag {
  //todo: check what these mean and that they are true
  def unpack(flags: Set[Int]): Map[SaneStatusFlag, Boolean] = Map(
    UsbPower -> flags(1),
    FirmwareLoaded -> flags(4)
  )

  implicit object SaneStatusFlagPrettyPrint extends PrettyPrint[SaneStatusFlag] {
    override def prettyPrint(value: SaneStatusFlag) = value.toString
  }
}
