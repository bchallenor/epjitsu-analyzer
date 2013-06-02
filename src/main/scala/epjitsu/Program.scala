package epjitsu

import java.io._
import scalaz._
import Scalaz._
import scala.collection.immutable.SortedSet
import epjitsu.util.PrettyPrint.BytesPrettyPrint
import epjitsu.util.XInstances._

object Program extends App {
  val unknownCommands = parsePcapFiles(new File(args(0)))
  println(s"Unknown commands: ${BytesPrettyPrint.prettyPrint(unknownCommands.toArray)}")

  def parsePcapFiles(dir: File): SortedSet[Byte] =  {
    val pcapFiles = dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(".pcap")
    })

    val unknownCommands = (pcapFiles.toList map (parsePcapFile(_))).concatenate
    unknownCommands
  }

  def parsePcapFile(pcapFile: File): SortedSet[Byte] = {
    val outputFile = new File(pcapFile.getParentFile, pcapFile.getName + ".log")
    println(s"Decoding $pcapFile to $outputFile...")

    val inputStream = new BufferedInputStream(new FileInputStream(pcapFile))
    val outputWriter = new FileWriter(outputFile)
    try {
      val packets = PcapFile.load(inputStream)

      val usbPackets = (packets collect { case PcapPacket(_, _, usb: UsbPacket) => usb }).toStream

      val bulkUsbPackets = usbPackets filter (_.xferType == UsbBulk)

      val singleDeviceBulkUsbPackets = discardAllButOneAddress(bulkUsbPackets)

      val bulkTransfers = UsbBulkTransferDecoder.decode(singleDeviceBulkUsbPackets)
//      bulkTransfers foreach { x =>
//        outputWriter.write(x.toString)
//        outputWriter.write('\n')
//      }

      val commands = Command.flagDuplicates(CommandPhraseDecoder.decode(bulkTransfers))
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

      assertNoPacketsMissing(singleDeviceBulkUsbPackets, commands)

      outputWriter.write("=" * 80)
      outputWriter.write('\n')

      val unknownCommands = (commands collect { case Command(PacketPhrase(_, UnknownCommandHeader(commandCode)), _, _) => commandCode }).to[SortedSet]
      outputWriter.write(s"Unknown commands: ${BytesPrettyPrint.prettyPrint(unknownCommands.toArray)}")
      outputWriter.write('\n')
      unknownCommands
    } finally {
      inputStream.close()
      outputWriter.close()
    }
  }

  def discardAllButOneAddress(bulkUsbPackets: Stream[UsbPacket]): Stream[UsbPacket] = {
    val addressToCount = bulkUsbPackets groupBy (x => (x.bus, x.device)) mapValues (_.size)
    val count = addressToCount.values.sum
    val addressCount = addressToCount.size

    val address = if (addressCount > 1) {
      val (mainAddress, mainAddressCount) = addressToCount.toSeq maxBy { case (_, count) => count }
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

  def assertNoPacketsMissing(originalPackets: Stream[UsbPacket], parsedCommands: Stream[Command]) {
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
