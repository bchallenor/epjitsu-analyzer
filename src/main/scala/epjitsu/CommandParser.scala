package epjitsu

import scala.util.parsing.combinator.Parsers
import java.nio.{ByteOrder, ByteBuffer}
import scala.collection.immutable.{SortedSet, BitSet}
import java.nio.charset.Charset
import scalaz._
import Scalaz._
import epjitsu.util.{DeepByteArray, PrettyPrint}
import epjitsu.Transfer.TransferPhrase

object CommandParser extends Parsers {
  type Elem = Transfer

  lazy val commandOrNone: Parser[Option[Command]] = command ?

  lazy val command: Parser[Command] =
    asCommand(sendCommandHeader(0x03, "get status flags"), receiveStatusFlags) |
    asCommand(sendCommandHeader(0x06, "load firmware"), receiveReturnCode, sendLengthPrefixedPayloadAndChecksum, receiveReturnCode) |
    asCommand(sendCommandHeader(0x13, "get identifiers"), receiveIdentifiers) |
    asCommand(sendCommandHeader(0x16, "re-init firmware"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0x24, "???"), receivePayload) |
    asCommand(sendCommandHeader(0x33, "get sensor flags"), receiveSensorFlags) |
    asCommand(sendCommandHeader(0x43, "get scan status"), receivePayload) |
    //asCommand(sendCommandHeader(0xb0, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0xb2, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0xb3, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0xb4, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0xb5, "???"), receiveReturnCode, receiveShort) |
    //asCommand(sendCommandHeader(0xb6, "???"), receiveReturnCode, receiveShort) |
    asCommand(sendCommandHeader(0xc3, "set fine cal #1"), receiveReturnCode, sendHeader, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc4, "set fine cal #2"), receiveReturnCode, sendHeader, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc5, "set lut"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xc6, "set coarse cal"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd0, "set lamp"), receiveReturnCode, sendBoolean, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd1, "set window"), receiveReturnCode, sendPayload, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd2, "get scan data #d2"), receiveReturnCode, receivePayload) |
    asCommand(sendCommandHeader(0xd3, "get scan data #d3"), receiveReturnCode, receivePayloadAndTrailer) |
    asCommand(sendCommandHeader(0xd4, "set paper feed"), receiveReturnCode, sendByte, receiveReturnCode) |
    asCommand(sendCommandHeader(0xd6, "start scan"), receiveReturnCode) |
    //asCommand(sendCommandHeader(0xd8, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    //asCommand(sendCommandHeader(0xe1, "???"), receiveReturnCode, sendByte, receiveReturnCode) |
    unknownCommand

  private lazy val unknownCommand: Parser[Command] =
    asCommand(sendAnyCommandHeader, anyNonCommand *)

  private def asCommand(headerParser: Parser[TransferPhrase[KnownCommandHeader]], bodyParsers: Parser[TransferPhrase[CommandBody[Any]]]*): Parser[Command] = {
    asCommand(headerParser, sequence1(bodyParsers.toList) ~ unexpectedNonCommands ^^ { case x ~ y => x ++ y }) |
      asCommand(headerParser, unexpectedNonCommands)
  }

  private def asCommand(headerParser: Parser[TransferPhrase[CommandHeader]], bodyParsers: Parser[List[TransferPhrase[CommandBody[Any]]]]): Parser[Command] = {
    headerParser ~ bodyParsers ^^ {
      case headerTransfer ~ bodyTransfers => Command(headerTransfer, bodyTransfers)
    }
  }

  private def sendCommandHeader(commandCode: Int, commandName: String): Parser[TransferPhrase[KnownCommandHeader]] =
    matchBytes(OutDir, f"Bytes matching {0x1b, 0x$commandCode%02x}", {
      case Array(0x1b, x) if (x & 0xff) == commandCode => KnownCommandHeader(x & 0xff, commandName)
    })

  private lazy val sendAnyCommandHeader: Parser[TransferPhrase[UnknownCommandHeader]] =
    matchBytes(OutDir, "Bytes matching {0x1b, _}", {
      case Array(0x1b, x) => UnknownCommandHeader(x & 0xff)
    })

  private lazy val unexpectedNonCommands: Parser[List[TransferPhrase[CommandBody[DeepByteArray]]]] =
    (anyNonCommand *) ^^ { nonCommandTransfers =>
      nonCommandTransfers foreach (x => println(s"Recovering from unexpected non-command: ${x.value}"))
      nonCommandTransfers
    }

  private lazy val anyNonCommand: Parser[TransferPhrase[CommandBody[DeepByteArray]]] =
    acceptMatch("Bytes not matching {0x1b, _}", Function.unlift(x => x.bytes match {
      case Array(0x1b, _) => None
      case _ => Some(PacketPhrase(SortedSet(x), CommandBody("unknown bytes", x.direction, DeepByteArray(x.bytes))))
    }))

  private lazy val receiveReturnCode = asCommandBody("return code", byte(InDir))
  private lazy val receiveShort = asCommandBody("short", short(InDir))
  private lazy val receiveStatusFlags = asCommandBody("status flags", bitSet(InDir, 16) ^^ lift(x => x -> SaneStatusFlag.unpack(x)))
  private lazy val receiveSensorFlags = asCommandBody("sensor flags", bitSet(InDir, 32) ^^ lift(x => x -> SaneSensorFlag.unpack(x)))
  private lazy val receiveIdentifiers = asCommandBody("manufacturer name -> product name", string(InDir) ^^ lift(x => (x.substring(0, 8).trim, x.substring(8, 32).trim)))
  private lazy val sendBoolean = asCommandBody("boolean", boolean(OutDir))
  private lazy val sendByte = asCommandBody("byte", byte(OutDir))
  private lazy val sendHeader = asCommandBody("header", singleTransferPayload(OutDir))
  private lazy val sendPayload = asCommandBody("payload", variableLengthPayload(OutDir))
  private lazy val receivePayload = asCommandBody("payload", variableLengthPayload(InDir))
  private lazy val receivePayloadAndTrailer = asCommandBody("payload -> trailer", variableLengthPayloadAndTrailer(InDir, 8))
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

  private def short(direction: TransferDir): Parser[TransferPhrase[Short]] =
    matchBytes(direction, "Single short", {
      case bytes if bytes.length == 2 => ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getShort
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

  private def singleTransferPayload(direction: TransferDir): Parser[TransferPhrase[DeepByteArray]] = bytes(direction) ^^ lift { bytes =>
    DeepByteArray(bytes)
  }

  private def variableLengthPayload(direction: TransferDir): Parser[TransferPhrase[DeepByteArray]] = (bytes(direction) +) ^^ { byteTransfers =>
    byteTransfers.sequence[TransferPhrase, Array[Byte]] map (x => DeepByteArray(x.toArray.flatten))
  }

  private def variableLengthPayloadAndTrailer(direction: TransferDir, trailerLength: Int): Parser[TransferPhrase[(DeepByteArray, DeepByteArray)]] = variableLengthPayload(direction) ^^ lift { x =>
      val payloadAndTrailerBytes = x.underlying
      assert(payloadAndTrailerBytes.length >= trailerLength, s"Expected payload of at least $trailerLength bytes, but found only ${payloadAndTrailerBytes.length} bytes")
      val (payloadBytes, trailerBytes) = payloadAndTrailerBytes.splitAt(payloadAndTrailerBytes.length - trailerLength)
      (DeepByteArray.make(payloadBytes), DeepByteArray.make(trailerBytes))
    }

  private def fixedLengthPayload(direction: TransferDir, length: Int): Parser[TransferPhrase[DeepByteArray]] = {
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
      byteTransfers.sequence[TransferPhrase, Array[Byte]] map (x => DeepByteArray(x.toArray.flatten))
    }
  }

  private def lengthPrefixedPayload(direction: TransferDir): Parser[TransferPhrase[DeepByteArray]] =
    int(direction) into { lengthTransfer =>
      fixedLengthPayload(direction, lengthTransfer.value) ^^ (payloadTransfer => lengthTransfer flatMap (_ => payloadTransfer))
    }

  private def lengthPrefixedPayloadAndChecksum(direction: TransferDir): Parser[TransferPhrase[(DeepByteArray, Byte)]] =
    lengthPrefixedPayload(direction) ~ byte(direction) ^^ { case x ~ y => ^(x, y)(_ -> _) }

  private def sequence1[T](parsers: List[Parser[T]]): Parser[List[T]] = {
    val zero: Parser[List[T]] = parsers.head map (List(_))
    val f: (Parser[List[T]], Parser[T]) => Parser[List[T]] = (acc, parser) => for { ts <- acc; t <- parser } yield t :: ts
    // parses in the correct direction but accumulates result backwards
    val reversedParser = parsers.tail.foldLeft(zero)(f)
    reversedParser ^^ (_.reverse)
  }

  private def lift[A, B](f: A => B): TransferPhrase[A] => TransferPhrase[B] = Functor[TransferPhrase].lift(f)
}
