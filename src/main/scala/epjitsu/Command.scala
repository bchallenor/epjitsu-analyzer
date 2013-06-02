package epjitsu

import scalaz._
import Scalaz._
import scala.util.parsing.input.Reader
import epjitsu.util.parsing._
import epjitsu.util.PrettyPrint
import epjitsu.Transfer.TransferPhrase

case class Command(headerTransfer: TransferPhrase[CommandHeader], bodyTransfers: List[TransferPhrase[CommandBody[Any]]], isDuplicate: Boolean = false) extends Packet {
  override lazy val seqNo = headerTransfer.packets.head.seqNo

  override def toString: String = toString(showUnderlying = false)

  def toString(showUnderlying: Boolean): String = {
    val sb = new StringBuilder()
    appendTransfer(sb, showUnderlying, headerTransfer)
    bodyTransfers foreach (appendTransfer(sb, showUnderlying, _))
    sb.toString()
  }

  private def appendTransfer(sb: StringBuilder, showUnderlying: Boolean, partTransfer: TransferPhrase[CommandPart]) {
    sb.append(partTransfer.value)
    sb.append("\n")

    if (showUnderlying) {
      partTransfer.packets foreach { packet =>
        sb.append(packet)
        sb.append("\n")
      }
    }
  }
}

object Command {
  def isDuplicate(prevCommand: Command, command: Command): Boolean = {
    (prevCommand.headerTransfer.value == command.headerTransfer.value) &&
      (prevCommand.bodyTransfers map (_.value)) == (command.bodyTransfers map (_.value))
  }

  def flagIfDuplicate(prevCommand: Command, command: Command): Command = {
    if (isDuplicate(prevCommand, command)) command.copy(isDuplicate = true) else command
  }

  def flagIfDuplicate(prevCommandOrNone: Option[Command], command: Command): Command = {
    prevCommandOrNone map (flagIfDuplicate(_, command)) getOrElse command
  }

  def flagDuplicates(prevCommandOrNone: Option[Command], commands: Stream[Command]): Stream[Command] = {
    commands match {
      case Stream.Empty => Stream.Empty
      case command #:: rest => flagIfDuplicate(prevCommandOrNone, command) #:: flagDuplicates(Some(command), rest)
    }
  }

  def flagDuplicates(commands: Stream[Command]): Stream[Command] = {
    flagDuplicates(None, commands)
  }
}

trait CommandPart {
  def desc: String
  override def toString: String = desc
}

trait CommandHeader extends CommandPart {
  def commandCode: Byte
  def commandName: String
  override lazy val desc = f"Command 0x$commandCode%02x: $commandName"
}

case class KnownCommandHeader(commandCode: Byte, commandName: String) extends CommandHeader

case class UnknownCommandHeader(commandCode: Byte) extends CommandHeader {
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
