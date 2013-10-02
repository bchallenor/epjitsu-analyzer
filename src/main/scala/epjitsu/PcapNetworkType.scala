package epjitsu

trait PcapNetworkType
case object LinuxUsbNetworkType extends PcapNetworkType
case object WindowsUsbNetworkType extends PcapNetworkType

object PcapNetworkType {
  def unapply(tag: Long): Option[PcapNetworkType] = tag match {
    case 220 => Some(LinuxUsbNetworkType)
    case 249 => Some(WindowsUsbNetworkType)
    case _ => None
  }
}
