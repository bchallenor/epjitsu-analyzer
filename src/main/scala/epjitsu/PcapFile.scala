package epjitsu

import epjitsu.utils.CloseableIterator
import java.io._
import java.nio.file.Path
import org.joda.time._
import scalaz._
import Scalaz._
import com.google.common.io.LittleEndianDataInputStream

object PcapFile {
  def load(path: Path): CloseableIterator[PcapPacket] = {
    val inputStream = new DataInputStream(new BufferedInputStream(new FileInputStream(path.toFile)))
    val dataInput = readHeaderMagicAndBuildDataInput(inputStream)
    val header = readHeader(dataInput)
    val decoder = networkTypeToDecoder.get(header.networkType) err s"No decoder for network type ${header.networkType}"
    new PcapPacketIterator(decoder, inputStream, dataInput)
  }

  private class PcapPacketIterator(decoder: PayloadDecoder, inputStream: InputStream, dataInput: DataInput) extends CloseableIterator[PcapPacket] {
    private var packetOrNone: Option[PcapPacket] = None

    override def hasNext: Boolean = {
      packetOrNone = try {
        val packet = readPacket(decoder, dataInput)
        Some(packet)
      } catch {
        case _: EOFException =>
          inputStream.close()
          None
      }
      packetOrNone.nonEmpty
    }

    override def next(): PcapPacket = packetOrNone err "No more packets"

    override def close() {
      inputStream.close()
    }
  }

  private def readHeaderMagicAndBuildDataInput(inputStream: InputStream): DataInput = {
    val bigEndianDataInput = new DataInputStream(inputStream)
    val magic = bigEndianDataInput.readInt()
    require(magic == 0xa1b2c3d4 || magic == 0xd4c3b2a1, s"Invalid magic in header: $magic")
    if (magic == 0xa1b2c3d4) bigEndianDataInput else new LittleEndianDataInputStream(inputStream)
  }
  
  private def readHeader(dataInput: DataInput): PcapHeader = {
    val major = dataInput.readUnsignedShort()
    val minor = dataInput.readUnsignedShort()
    val utcOffset = dataInput.readInt()
    val timestampSigFigs = dataInput.readInt()
    val maxPacketLength = dataInput.readInt()
    val networkType = dataInput.readInt()
    PcapHeader(major, minor, utcOffset, timestampSigFigs, maxPacketLength, networkType)
  }

  private def readPacket(decoder: PayloadDecoder, dataInput: DataInput): PcapPacket = {
    val timestampSecs = dataInput.readInt()
    val timestampMicros = dataInput.readInt()
    val timestamp = new DateTime(timestampSecs * 1000L + timestampMicros / 1000L)

    val includedLength = dataInput.readInt()
    val originalLength = dataInput.readInt()
    if (includedLength != originalLength) sys.error(s"Missing packet data: capture includes only $includedLength bytes of $originalLength")

    val payload = decoder.decode(dataInput)

    PcapPacket(timestamp, payload)
  }

  private val networkTypeToDecoder: Map[Int, PayloadDecoder] = Map(
    220 -> LinuxUsbPayloadDecoder
  )
}

sealed case class PcapHeader(major: Int, minor: Int, utcOffset: Int, timestampSigFigs: Int, maxPacketLength: Int, networkType: Int)
sealed case class PcapPacket(timestamp: DateTime, payload: Payload)
