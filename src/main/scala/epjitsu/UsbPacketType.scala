package epjitsu

sealed trait UsbPacketType
case object UsbSubmit extends UsbPacketType
case object UsbComplete extends UsbPacketType
