package epjitsu

import epjitsu.utils.CloseableIterator
import java.io._
import java.nio.file.Path
import scalaz._
import Scalaz._

object PcapFile {
  def load(path: Path): CloseableIterator[PcapPacket] = {
    val in = new DataInputStream(new BufferedInputStream(new FileInputStream(path.toFile)))
    val h = readHeader(in)
    new PcapPacketIterator(h, in)
  }

  private class PcapPacketIterator(header: PcapHeader, in: DataInputStream) extends CloseableIterator[PcapPacket] {
    private var packetOrNone: Option[PcapPacket] = None

    override def hasNext: Boolean = {
      packetOrNone = try {
        val packet = readPacket(in, header.swap)
        Some(packet)
      } catch {
        case _: EOFException =>
          in.close()
          None
      }
      packetOrNone.nonEmpty
    }

    override def next(): PcapPacket = packetOrNone err "No more packets"

    override def close() {
      in.close()
    }
  }

  private def readHeader(in: DataInputStream): PcapHeader = {
    val magic = in.readInt()
    require(magic == 0xa1b2c3d4 || magic == 0xd4c3b2a1, s"Invalid magic in header: $magic")
    val swap = magic == 0xd4c3b2a1
    val major = readInt16(in, swap)
    val minor = readInt16(in, swap)
    val utcOffset = readInt32(in, swap)
    val timestampSigFigs = readInt32(in, swap)
    val maxPacketLength = readInt32(in, swap)
    val networkType = readInt32(in, swap)
    PcapHeader(swap, major, minor, utcOffset, timestampSigFigs, maxPacketLength, networkType)
  }

  private def readPacket(in: DataInputStream, swap: Boolean): PcapPacket = {
    val timestampSecs = readInt32(in, swap)
    val timestampMicros = readInt32(in, swap)
    val includedLength = readInt32(in, swap)
    val originalLength = readInt32(in, swap)
    val bytes = Array.ofDim[Byte](includedLength)
    in.readFully(bytes)
    PcapPacket(timestampSecs, timestampMicros, includedLength, originalLength, bytes)
  }

  private def readInt16(in: DataInputStream, swap: Boolean): Int = {
    val i = in.readUnsignedShort()
    if (swap) {
      (((i >> 8) & 0xff) << 0) |
        (((i >> 0) & 0xff) << 8)
    } else {
      i
    }
  }

  private def readInt32(in: DataInputStream, swap: Boolean): Int = {
    val i = in.readInt()
    if (swap) {
      (((i >> 24) & 0xff) <<  0) |
        (((i >> 16) & 0xff) <<  8) |
        (((i >>  8) & 0xff) << 16) |
        (((i >>  0) & 0xff) << 24)
    } else {
      i
    }
  }
}

case class PcapHeader(swap: Boolean, major: Int, minor: Int, utcOffset: Int, timestampSigFigs: Int, maxPacketLength: Int, networkType: Int)
case class PcapPacket(timestampSecs: Int, timestampMicros: Int, includedLength: Int, originalLength: Int, bytes: Array[Byte])
