package epjitsu

import org.joda.time.DateTime
import java.io.{EOFException, DataInput}

class PcapPacketStreamDecoder[P <: PcapPacket, Q <: Packet](decoder: PacketDecoder[(DateTime, DataInput), P], translator: PacketStreamTranslator[P, Q]) extends PacketStreamDecoder[DataInput, Q] {
  override def decode(input: DataInput): Stream[Q] = {
    val intermediateStream: Stream[P] = decodeIntermediate(0L, input)
    translator.translate(intermediateStream)
  }

  private def decodeIntermediate(seqNo: Long, input: DataInput): Stream[P] = {
    val timestampSecs = input.readInt()
    val timestampMicros = input.readInt()
    val timestamp = new DateTime(timestampSecs * 1000L + timestampMicros / 1000L)

    val includedLength = input.readInt()
    val originalLength = input.readInt()
    if (includedLength != originalLength) sys.error(s"Missing packet data: capture includes packet with only $includedLength bytes of $originalLength")

    val packet = decoder.decode(seqNo, (timestamp, input))

    packet #:: {
      try decodeIntermediate(seqNo + 1L, input)
      catch { case _: EOFException => Stream.Empty }
    }
  }
}
