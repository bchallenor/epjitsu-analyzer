package epjitsu

import java.io.{FileInputStream, BufferedInputStream}
import scala.collection.immutable.SortedSet

object Program extends App {
  val inputStream = new BufferedInputStream(new FileInputStream(args(0)))
  try {
    val packets = PcapFile.load(inputStream)

    val usbPackets = (packets collect { case PcapPacket(_, _, usb: UsbPacket) => usb }).toStream

    val bulkUsbPackets = usbPackets filter (_.xferType == UsbBulk)

    val bulkTransfers = UsbBulkTransferDecoder.decode(bulkUsbPackets) map (_.value)

    val saneCommands = SaneCommandPhraseDecoder.decode(bulkTransfers) map (_.value)
    saneCommands foreach (x => println(s"$x\n"))

    val unknownCommands = (saneCommands collect { case SaneCommand(_, SaneUnknownCommandResult(command, _, _)) => command }).distinct.sorted.toList
    val unknownCommandsStr = unknownCommands map ("0x%02x" format _) mkString("{", ", ", "}")
    println(s"Unknown commands: $unknownCommandsStr")
  } finally {
    inputStream.close()
  }
}
