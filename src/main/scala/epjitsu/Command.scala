package epjitsu

import scalaz._
import Scalaz._
import scala.util.parsing.input.Reader
import epjitsu.util.parsing._
import epjitsu.util.PrettyPrint
import epjitsu.Transfer.TransferPhrase

case class Command(headerTransfer: TransferPhrase[CommandHeader], bodyTransfers: List[TransferPhrase[CommandBody[Any]]]) extends Packet {
  override lazy val seqNo = headerTransfer.packets.head.seqNo

  override def toString: String = {
    val sb = new StringBuilder()
    appendTransfer(sb, headerTransfer)
    bodyTransfers foreach (appendTransfer(sb, _))
    sb.toString()
  }

  private def appendTransfer(sb: StringBuilder, partTransfer: TransferPhrase[CommandPart]) {
    sb.append(partTransfer.value)
    sb.append("\n")

    partTransfer.packets foreach { transfer =>
      sb.append(transfer)
      sb.append("\n")
    }
  }
}

trait CommandPart {
  def desc: String
  override def toString: String = desc
}

trait CommandHeader extends CommandPart {
  def commandCode: Int
  def commandName: String
  override lazy val desc = f"Command 0x$commandCode%02x: $commandName"
}

case class KnownCommandHeader(commandCode: Int, commandName: String) extends CommandHeader

case class UnknownCommandHeader(commandCode: Int) extends CommandHeader {
  override def commandName: String = "unknown"
}

case class CommandBody[+A](name: String, direction: TransferDir, value: A)(implicit pp: PrettyPrint[A]) extends CommandPart {
  private def directionStr = direction match {
    case OutDir => "Sent"
    case InDir => "Received"
  }

  override lazy val desc = f"$directionStr $name: ${pp.prettyPrint(value)}"
}

object CommandPhraseDecoder extends PacketStreamDecoder[Transfer, Command] {
  override def decode(transfers: Stream[Transfer]): Stream[Command] = {
    decodePhrase(transfers)
  }

  private def decodePhrase(in: Reader[Transfer]): Stream[Command] = {
    CommandParser.commandOrNone(in) match {
      case CommandParser.Success(commandOrNone, rest) => commandOrNone map (_ #:: decodePhrase(rest)) getOrElse Stream.Empty
      case CommandParser.NoSuccess(msg, _) => sys.error(msg)
    }
  }
}
