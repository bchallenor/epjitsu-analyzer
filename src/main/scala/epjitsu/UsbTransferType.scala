package epjitsu

sealed trait UsbTransferType
case object UsbIsochronous extends UsbTransferType
case object UsbInterrupt extends UsbTransferType
case object UsbControl extends UsbTransferType
case object UsbBulk extends UsbTransferType
