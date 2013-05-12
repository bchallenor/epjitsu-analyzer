package epjitsu

import java.io._
import scalaz._
import Scalaz._
import com.google.common.io.LittleEndianDataInputStream

object PcapFile {
  def load(inputStream: InputStream): Iterator[PcapPacket] = {
    val dataInput = readHeaderMagicAndBuildDataInput(inputStream)
    val header = readHeader(dataInput)
    val decoder = new PcapPacketDecoder(header.networkType)
    new PcapPacketIterator(decoder, dataInput)
  }

  private class PcapPacketIterator(decoder: PacketDecoder[DataInput, PcapPacket], dataInput: DataInput) extends Iterator[PcapPacket] {
    private var packetOrNone: Option[PcapPacket] = None
    private var seqNo: Long = 0

    override def hasNext: Boolean = {
      packetOrNone = try {
        val packet = decoder.decode(seqNo, dataInput)
        seqNo += 1
        Some(packet)
      } catch {
        case _: EOFException => None
      }
      packetOrNone.nonEmpty
    }

    override def next(): PcapPacket = packetOrNone err "No more packets"
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

  private case class PcapHeader(major: Int, minor: Int, utcOffset: Int, timestampSigFigs: Int, maxPacketLength: Int, networkType: Int)
}
