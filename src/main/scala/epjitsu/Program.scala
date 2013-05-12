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

    val saneTransfers = SaneTransferPhraseDecoder.decode(bulkUsbPackets)

    val saneCommands = SaneCommandPhraseDecoder.decode(saneTransfers)
    saneCommands foreach (x => println(s"$x\n"))

    val unknownCommands = (saneCommands collect { case SaneCommandPhrase(_, SaneUnknownCommandResult(command, _, _)) => command }).distinct.sorted.toList
    val unknownCommandsStr = unknownCommands map ("0x%02x" format _) mkString("{", ", ", "}")
    println(s"Unknown commands: $unknownCommandsStr")
  } finally {
    inputStream.close()
  }
}
