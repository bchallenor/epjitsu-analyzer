package epjitsu

import java.io._
import scala.collection.immutable.SortedSet
import epjitsu.util.{PrettyPrint, DeepByteArray}

object Analyzer {
  def analyzePcapFile(pcapFile: File): Stream[Command] = {
    println(s"Decoding $pcapFile...")
    val inputStream = new BufferedInputStream(new FileInputStream(pcapFile))
    try {
      val packets = PcapFile.load(inputStream)

      val usbPackets = (packets collect { case PcapPacket(_, _, usb: UsbPacket) => usb }).toStream

      val bulkUsbPackets = usbPackets filter (_.xferType == UsbBulk)

      val singleDeviceBulkUsbPackets = discardAllButOneAddress(bulkUsbPackets)

      val bulkTransfers = UsbBulkTransferDecoder.decode(singleDeviceBulkUsbPackets)

      val commands = Command.flagDuplicates(CommandPhraseDecoder.decode(bulkTransfers))

      assertNoPacketsMissing(singleDeviceBulkUsbPackets, commands)

      commands
    } finally {
      inputStream.close()
    }
  }

  def collectUnknownCommands(commands: Stream[Command]): Set[Command] = {
    val unknownCommands = (commands collect { case command @ Command(PacketPhrase(_, _: UnknownCommandHeader), _, _) => command.withoutUnderlying }).toSet
    unknownCommands
  }

  def logCommands(commands: Stream[Command], outputFile: File) {
    println(s"Logging commands to $outputFile...")
    val outputWriter = new FileWriter(outputFile)
    try {
      commands foreach { x =>
        if (x.isDuplicate) {
          // Don't print duplicates
          val commandCode = x.headerTransfer.value.commandCode
          assert(commandCode == 0x03 || commandCode == 0x33, s"Expected only status/sensor flag commands to be duplicates: ${x.headerTransfer.value}")
        } else {
          outputWriter.write(x.toString(showUnderlying = false))
          outputWriter.write('\n')
        }
      }
    } finally {
      outputWriter.close()
    }
  }

  def logUnknownCommands(unknownCommands: Set[Command], outputFile: File) {
    println(s"Logging unknown commands to $outputFile...")
    val outputWriter = new FileWriter(outputFile)
    try {
      (unknownCommands map (_.toString(showUnderlying = false))).toSeq.sorted foreach { x =>
        outputWriter.write(x)
        outputWriter.write('\n')
      }
    } finally {
      outputWriter.close()
    }
  }

  private def discardAllButOneAddress(bulkUsbPackets: Stream[UsbPacket]): Stream[UsbPacket] = {
    val addressToCount = bulkUsbPackets groupBy (x => (x.bus, x.device)) mapValues (_.size)
    val count = addressToCount.values.sum
    val addressCount = addressToCount.size

    val address = if (addressCount > 1) {
      val (mainAddress, mainAddressCount) = addressToCount.toSeq maxBy { case (_, count0) => count0 }
      val mainAddressPercent = mainAddressCount * 100.0 / count
      println(f"Found $addressCount distinct addresses. Choosing address $mainAddress with $mainAddressPercent%.1f% of the packets.")
      mainAddress
    } else if (addressCount == 1) {
      addressToCount.keys.head
    } else {
      sys.error("Found no bulk USB packets")
    }

    bulkUsbPackets filter (x => (x.bus, x.device) == address)
  }

  private def assertNoPacketsMissing(originalPackets: Stream[UsbPacket], parsedCommands: Stream[Command]) {
    val originalSeqNos: Set[Long] = (originalPackets.iterator map (_.seqNo)).to[SortedSet]

    val parsedSeqNos: Set[Long] = (for {
      command <- parsedCommands.iterator
      commandPart <- command.headerTransfer :: command.bodyTransfers
      transfer @ (_transfer: UsbBulkTransfer) <- commandPart.packets
      packet <- transfer.hostPacket :: transfer.devicePacket :: Nil
    } yield packet.seqNo).to[SortedSet]

    assert(originalSeqNos == parsedSeqNos, s"Expected seq numbers to be the same after parsing, but found additional: ${parsedSeqNos -- originalSeqNos}, missing: ${originalSeqNos -- parsedSeqNos}")
  }
}
