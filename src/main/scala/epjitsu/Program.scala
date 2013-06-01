package epjitsu

import java.io.{FileInputStream, BufferedInputStream}

object Program extends App {
  val inputStream = new BufferedInputStream(new FileInputStream(args(0)))
  try {
    val packets = PcapFile.load(inputStream)

    val usbPackets = (packets collect { case PcapPacket(_, _, usb: UsbPacket) => usb }).toStream

    val bulkUsbPackets = usbPackets filter (_.xferType == UsbBulk)

    val bulkTransfers = UsbBulkTransferDecoder.decode(bulkUsbPackets)

    val commands = CommandPhraseDecoder.decode(bulkTransfers)
    commands foreach (println(_))

    assertNoPacketsMissing(bulkUsbPackets, commands)

    val unknownCommands = (commands collect { case Command(PacketPhrase(_, UnknownCommandHeader(commandCode)), _) => commandCode }).distinct.sorted.toList
    val unknownCommandsStr = unknownCommands map ("0x%02x" format _) mkString("{", ", ", "}")
    println(s"Unknown commands: $unknownCommandsStr")
  } finally {
    inputStream.close()
  }

  def assertNoPacketsMissing(originalPackets: Stream[UsbPacket], parsedCommands: Stream[Command]) {
    val originalSeqNos: Set[Long] = (originalPackets.iterator map (_.seqNo)).toSet

    val parsedSeqNos: Set[Long] = (for {
      command <- parsedCommands.iterator
      commandPart <- command.headerTransfer :: command.bodyTransfers
      transfer @ (_transfer: UsbBulkTransfer) <- commandPart.packets
      packet <- transfer.hostPacket :: transfer.devicePacket :: Nil
    } yield packet.seqNo
      ).toSet

    assert(originalSeqNos == parsedSeqNos, s"Expected seq numbers to be the same after parsing, but found additional: ${parsedSeqNos -- originalSeqNos}, missing: ${originalSeqNos -- parsedSeqNos}")
  }
}
