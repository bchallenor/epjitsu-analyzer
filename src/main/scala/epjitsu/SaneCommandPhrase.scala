package epjitsu

import UsbBulkTransfer.UsbBulkTransferPhrase
import scalaz._
import Scalaz._
import scala.collection.immutable.SortedSet

case class SaneCommand(transfers: SortedSet[UsbBulkTransfer], commandResult: SaneCommandResult) extends Packet {
  val seqNo: Long = transfers.head.seqNo
}

object SaneCommand {
  def apply(transfers: SortedSet[UsbBulkTransfer]): SaneCommand = {
    val commandTransfer = transfers.headOption err "Expected at least one transfer"
    val command = getCommandOrNone(commandTransfer) err s"Expected first transfer to be command: $commandTransfer"
    val otherTransfers = transfers.tail
    val commandResult = SaneCommandResult(command, commandTransfer, otherTransfers)
    SaneCommand(transfers, commandResult)
  }

  def isCommand(transfer: UsbBulkTransfer): Boolean = getCommandOrNone(transfer).isDefined

  def getCommandOrNone(transfer: UsbBulkTransfer): Option[Int] = {
    if (transfer.direction == OutDir) {
      transfer.bytes match {
        case Array(0x1b, cmd) => Some(cmd & 0xff)
        case _ => None
      }
    } else {
      None
    }
  }
}

object SaneCommandPhraseDecoder extends PacketPhraseDecoder[UsbBulkTransfer, SaneCommand] {
  def decode(transfers: Stream[UsbBulkTransfer]): Stream[UsbBulkTransferPhrase[SaneCommand]] = {
    // Split transfers at commands
    val splitAtCommands = splitBefore(transfers, (x: UsbBulkTransfer) => SaneCommand.isCommand(x))
    splitAtCommands map (_.to[SortedSet]) map (ps => PacketPhrase(ps, SaneCommand(ps)))
  }

  private def splitBefore[A](elements: Stream[A], predicate: A => Boolean): Stream[List[A]] = {
    elements match {
      case head #:: rest =>
        require(predicate(head), s"Expected head to conform to predicate: $head")
        val fails = (rest takeWhile (!predicate(_))).toList
        (head :: fails) #:: splitBefore (rest drop (fails.length), predicate)

      case Stream.Empty =>
        Stream.Empty
    }
  }
}
