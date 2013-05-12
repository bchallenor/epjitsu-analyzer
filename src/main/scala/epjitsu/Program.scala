package epjitsu

import java.io.{FileInputStream, BufferedInputStream}

object Program extends App {
  val inputStream = new BufferedInputStream(new FileInputStream(args(0)))
  try {
    val packets = PcapFile.load(inputStream)

    val usbPackets = (packets collect { case PcapPacket(_, _, usb: UsbPacket) => usb }).toStream

    val bulkUsbPackets = usbPackets filter (_.xferType == UsbBulk)

    val distinctDevices = (bulkUsbPackets map (x => (x.bus, x.device))).distinct
    assert(distinctDevices.size <= 1, s"Expected one device only: $distinctDevices")

    val sanePackets = SaneTransferPhraseDecoder.decode(bulkUsbPackets)
    sanePackets foreach (println(_))
  } finally {
    inputStream.close()
  }
}
