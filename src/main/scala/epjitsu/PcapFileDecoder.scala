package epjitsu

import java.io._
import scalaz._
import Scalaz._
import com.google.common.io.LittleEndianDataInputStream

class PcapFileDecoder[Q <: Packet](packetDecoders: Map[PcapNetworkType, PacketStreamDecoder[DataInput, Q]]) extends PacketStreamDecoder[InputStream, Q] {
  override def decode(inputStream: InputStream): Stream[Q] = {
    val dataInput = readHeaderMagicAndBuildDataInput(inputStream)
    val header = readHeader(dataInput)
    import header._
    val packetDecoder = PcapNetworkType unapply networkTypeTag flatMap packetDecoders.get err s"$networkTypeTag is not a known network type"
    packetDecoder.decode(dataInput)
  }

  private def readHeaderMagicAndBuildDataInput(inputStream: InputStream): DataInput = {
    val bigEndianDataInput = new DataInputStream(inputStream)
    val magic = bigEndianDataInput.readInt()
    require(magic == 0xa1b2c3d4 || magic == 0xd4c3b2a1, s"Invalid magic in header: $magic")
    if (magic == 0xa1b2c3d4) bigEndianDataInput else new LittleEndianDataInputStream(inputStream)
  }

  private def readHeader(dataInput: DataInput): PcapFileHeader = {
    val major = dataInput.readUnsignedShort()
    val minor = dataInput.readUnsignedShort()
    val utcOffset = dataInput.readInt()
    val timestampSigFigs = dataInput.readInt()
    val maxPacketLength = dataInput.readInt()
    val networkTypeTag = dataInput.readInt()
    PcapFileHeader(major, minor, utcOffset, timestampSigFigs, maxPacketLength, networkTypeTag)
  }

  private case class PcapFileHeader(major: Int, minor: Int, utcOffset: Int, timestampSigFigs: Int, maxPacketLength: Int, networkTypeTag: Int)
}
