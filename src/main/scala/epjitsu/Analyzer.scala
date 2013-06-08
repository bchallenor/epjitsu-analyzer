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

  def logHeaderMagic(commands: Stream[Command], outputFile: File, inRes: InRes.Value) {
    println(s"Logging header magic to $outputFile...")
    val outputWriter = new FileWriter(outputFile)
    try {
      outputWriter.write(s"/*************** XXX ${inRes}dpi *************/")
      outputWriter.write('\n')

      logSetWindowMagic(commands, outputWriter, "coarse cal", "CoarseCal", 0xc6, inRes)
      logSetWindowMagic(commands, outputWriter, "fine cal", "FineCal", 0xd2, inRes)
      logSetWindowMagic(commands, outputWriter, "gain/offset tables", "SendCal", 0xc3, inRes)
      logSendCalHeaderMagic(commands, outputWriter, "gain?", "Cal1", 0xc3, inRes)
      logSendCalHeaderMagic(commands, outputWriter, "offset?", "Cal2", 0xc4, inRes)
      logSetWindowMagic(commands, outputWriter, "scan", "Scan", 0xd6, inRes)
    } finally {
      outputWriter.close()
    }
  }

  private def logSetWindowMagic(commands: Stream[Command], outputWriter: FileWriter, humanName: String, varName: String, nextCommandCode: Int, inRes: InRes.Value) {
    commands sliding 2 collectFirst {
      case Stream(
        Command(PacketPhrase(_, KnownCommandHeader(0xd1, _)), List(_, PacketPhrase(_, CommandBody(_, OutDir, magic: DeepByteArray)), _), _),
        Command(PacketPhrase(_, KnownCommandHeader(x, _)), _, _)
      ) if x == nextCommandCode =>
        println(f"Found set window magic before $humanName ($nextCommandCode%02x)")
        outputWriter.write(f"/* 1b d1 (set window) before $humanName ($nextCommandCode%02x) */")
        outputWriter.write('\n')
        outputWriter.write(s"static unsigned char setWindow${varName}_XXX_$inRes[] = ${PrettyPrint.BytesPrettyPrint.prettyPrint(magic)};")
        outputWriter.write('\n')
    }
  }

  private def logSendCalHeaderMagic(commands: Stream[Command], outputWriter: FileWriter, humanName: String, varName: String, commandCode: Int, inRes: InRes.Value) {
    commands collectFirst {
      case Command(PacketPhrase(_, KnownCommandHeader(x, _)), List(
        _,
        PacketPhrase(_, CommandBody(_, OutDir, header: DeepByteArray)),
        PacketPhrase(_, CommandBody(_, OutDir, payload: DeepByteArray)),
        _
      ), _) if x == commandCode =>
        println(f"Found $commandCode%02x ($humanName) command header")
        outputWriter.write(f"/* 1b $commandCode%02x ($humanName) command header */")
        outputWriter.write('\n')
        val payloadLength = payload.underlying.length
        val pretty = PrettyPrint.BytesPrettyPrint.prettyPrint(header).replace("{", f"{ /* plus $payloadLength (0x$payloadLength%x) data bytes */")
        outputWriter.write(s"static unsigned char send${varName}Header_XXX_$inRes[] = $pretty;")
        outputWriter.write('\n')
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
