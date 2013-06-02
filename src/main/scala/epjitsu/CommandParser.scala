package epjitsu

import scala.util.parsing.combinator.Parsers
import java.nio.{ByteOrder, ByteBuffer}
import scala.collection.immutable.{SortedSet, BitSet}
import java.nio.charset.Charset
import scalaz._
import Scalaz._
import scala.annotation.tailrec
import epjitsu.util.PrettyPrint
import epjitsu.Transfer.TransferPhrase

object CommandParser extends Parsers {
  type Elem = Transfer

  lazy val commandOrNone: Parser[Option[Command]] = command ?

  lazy val command: Parser[Command] =
    asCommand(sendCommandHeader(0x03, "get status flags"), receiveStatusFlags) |
    asCommand(sendCommandHeader(0x06, "load firmware"), receiveReturnCode, sendLengthPrefixedPayloadAndChecksum, receiveReturnCode) |
    asCommand(sendCommandHeader(0x13, "get identifiers"), receiveIdentifiers) |
    asCommand(sendCommandHeader(0x16, "re-init firmware"), receiveReturnCode, sendByte, receiveReturnCode) |
    // 0x24
    asCommand(sendCommandHeader(0x33, "get sensor flags"), receiveSensorFlags) |
    asCommand(sendCommandHeader(0x43, "get scan status"), receivePayload) |
    // 0xb0
    // 0xb2
    // 0xb3
    // 0xb4
    // 0xb5
    // 0xb6
    asCommand(sendCommandHeader(0xc3, "set fine cal #1"), receiveReturnCode, sendHeader, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc4, "set fine cal #2"), receiveReturnCode, sendHeader, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc5, "set lut"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc6, "set coarse cal"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd0, "set lamp"), receiveReturnCode, sendBoolean, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd1, "set window"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd2, "get scan data #d2"), receiveReturnCode, receivePayload) |
    asCommand(sendCommandHeader(0xd3, "get scan data #d3"), receiveReturnCode, receivePayload) |
    asCommand(sendCommandHeader(0xd4, "ingest paper"), receiveReturnCode, sendBoolean, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd6, "get scan data #d6"), receiveReturnCode, receivePayload) |
    // 0xd8
    // 0xe1
    unknownCommand

  private lazy val unknownCommand: Parser[Command] =
    asCommand(sendAnyCommandHeader, anyNonCommand *)

  private def asCommand(headerParser: Parser[TransferPhrase[KnownCommandHeader]], bodyParsers: Parser[TransferPhrase[CommandBody[Any]]]*): Parser[Command] = {
    asCommand(headerParser, sequence1(bodyParsers.toList)) |
      asCommand(headerParser, anyNonCommand *) // recover if the body didn't meet our expectations
  }

  private def asCommand(headerParser: Parser[TransferPhrase[CommandHeader]], bodyParsers: Parser[List[TransferPhrase[CommandBody[Any]]]]): Parser[Command] = {
    headerParser ~ bodyParsers ^^ {
      case headerTransfer ~ bodyTransfers => Command(headerTransfer, bodyTransfers)
    }
  }

  private def sendCommandHeader(commandCode: Int, commandName: String): Parser[TransferPhrase[KnownCommandHeader]] =
    matchBytes(OutDir, f"Bytes matching {0x1b, 0x$commandCode%02x}", {
      case Array(0x1b, x) if x == commandCode.toByte => KnownCommandHeader(x, commandName)
    })

  private lazy val sendAnyCommandHeader: Parser[TransferPhrase[UnknownCommandHeader]] =
    matchBytes(OutDir, "Bytes matching {0x1b, _}", {
      case Array(0x1b, x) => UnknownCommandHeader(x)
    })

  private lazy val anyNonCommand: Parser[TransferPhrase[CommandBody[Array[Byte]]]] =
    acceptMatch("Bytes not matching {0x1b, _}", Function.unlift(x => x.bytes match {
      case Array(0x1b, _) => None
      case _ => Some(PacketPhrase(SortedSet(x), CommandBody("unknown non-command", x.direction, x.bytes)))
    }))

  private lazy val receiveReturnCode = asCommandBody("return code", byte(InDir))
  private lazy val receiveStatusFlags = asCommandBody("status flags", bitSet(InDir, 16) ^^ lift(x => x -> SaneStatusFlag.unpack(x)))
  private lazy val receiveSensorFlags = asCommandBody("sensor flags", bitSet(InDir, 32) ^^ lift(x => x -> SaneSensorFlag.unpack(x)))
  private lazy val receiveIdentifiers = asCommandBody("manufacturer name -> product name", string(InDir) ^^ lift(x => (x.substring(0, 8).trim, x.substring(8, 32).trim)))
  private lazy val sendBoolean = asCommandBody("boolean", boolean(OutDir))
  private lazy val sendByte = asCommandBody("byte", byte(OutDir))
  private lazy val sendHeader = asCommandBody("header", bytes(OutDir))
  private lazy val sendPayload = asCommandBody("payload", payload(OutDir))
  private lazy val receivePayload = asCommandBody("payload", payload(InDir))
  private lazy val sendLengthPrefixedPayloadAndChecksum = asCommandBody("payload -> checksum", lengthPrefixedPayloadAndChecksum(OutDir))

  private def asCommandBody[A: PrettyPrint](name: String, valueParser: Parser[TransferPhrase[A]]): Parser[TransferPhrase[CommandBody[A]]] = valueParser ^^ { transfer =>
    val directions = transfer.packets map (_.direction)
    val direction = if (directions.size == 1) directions.head else sys.error("Transfers in a CommandBody must all be in the same direction")
    transfer map (value => CommandBody(name, direction, value))
  }

  private def matchBytes[A](direction: TransferDir, matchDesc: String, matcher: PartialFunction[Array[Byte], A]): Parser[TransferPhrase[A]] =
    acceptMatch(s"$matchDesc in $direction direction", {
      case transfer if transfer.direction == direction && matcher.isDefinedAt(transfer.bytes) => PacketPhrase(SortedSet(transfer), matcher(transfer.bytes))
    })

  private def bytes(direction: TransferDir): Parser[TransferPhrase[Array[Byte]]] =
    matchBytes(direction, "Any bytes", {
      case x => x
    })

  private def byte(direction: TransferDir): Parser[TransferPhrase[Byte]] =
    matchBytes(direction, "Single byte", {
      case Array(x) => x
    })

  private def boolean(direction: TransferDir): Parser[TransferPhrase[Boolean]] =
    matchBytes(direction, "Single boolean", {
      case Array(0x00) => false
      case Array(0x01) => true
    })

  private def int(direction: TransferDir): Parser[TransferPhrase[Int]] =
    matchBytes(direction, "Single int", {
      case bytes if bytes.length == 4 => ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getInt
    })

  private def bitSet(direction: TransferDir, bits: Int): Parser[TransferPhrase[BitSet]] =
    matchBytes(direction, s"Bit set with $bits bits", {
      case bytes if bytes.length == (bits / 8) =>
        val paddedBytes = Array.ofDim[Byte]((bytes.length + 7) & ~0x07)
        bytes.copyToArray(paddedBytes)

        val lb = ByteBuffer.wrap(paddedBytes).order(ByteOrder.LITTLE_ENDIAN).asLongBuffer()
        val longs = Array.ofDim[Long](lb.remaining())
        lb.get(longs)

        BitSet.fromBitMaskNoCopy(longs)
    })

  private def string(direction: TransferDir): Parser[TransferPhrase[String]] =
    matchBytes(direction, "String", {
      case x => Charset.forName("UTF-8").decode(ByteBuffer.wrap(x)).toString
    })

  private def payload(direction: TransferDir): Parser[TransferPhrase[Array[Byte]]] = (bytes(direction) +) ^^ { byteTransfers =>
    byteTransfers.sequence[TransferPhrase, Array[Byte]] map (_.toArray.flatten)
  }

  private def fixedLengthPayload(direction: TransferDir, length: Int): Parser[TransferPhrase[Array[Byte]]] = {
    var remaining = length - 1 // checksum handled separately

    val bytesParser: Parser[TransferPhrase[Array[Byte]]] = bytes(direction) ^^ { bytes =>
      remaining -= bytes.value.length
      bytes
    }

    val separatorParser: Parser[Unit] = Parser { in =>
      if (remaining > 0) Success((), in)
      else if (remaining == 0) Failure("No bytes remaining", in)
      else Error(s"Expected remaining to be positive or zero: $remaining", in)
    }

    rep1sep(bytesParser, separatorParser) ^^ { byteTransfers =>
      byteTransfers.sequence[TransferPhrase, Array[Byte]] map (_.toArray.flatten)
    }
  }

  private def lengthPrefixedPayload(direction: TransferDir): Parser[TransferPhrase[Array[Byte]]] =
    int(direction) into { lengthTransfer =>
      fixedLengthPayload(direction, lengthTransfer.value) ^^ (payloadTransfer => lengthTransfer flatMap (_ => payloadTransfer))
    }

  private def lengthPrefixedPayloadAndChecksum(direction: TransferDir): Parser[TransferPhrase[(Array[Byte], Byte)]] =
    lengthPrefixedPayload(direction) ~ byte(direction) ^^ { case x ~ y => ^(x, y)(_ -> _) }

  private def sequence1[T](parsers: List[Parser[T]]): Parser[List[T]] = {
    val zero: Parser[List[T]] = parsers.head map (List(_))
    val f: (Parser[List[T]], Parser[T]) => Parser[List[T]] = (acc, parser) => for { ts <- acc; t <- parser } yield (t :: ts)
    // parses in the correct direction but accumulates result backwards
    val reversedParser = parsers.tail.foldLeft(zero)(f)
    reversedParser ^^ (_.reverse)
  }

  private def lift[A, B](f: A => B): TransferPhrase[A] => TransferPhrase[B] = Functor[TransferPhrase].lift(f)
}
