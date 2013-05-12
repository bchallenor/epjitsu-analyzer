package epjitsu

import scalaz._
import Scalaz._

case class SaneCommandPhrase(transfers: List[SaneTransferPhrase], commandResult: SaneCommandResult) extends PacketPhrase[SaneTransferPhrase] {
  override def packets = transfers
  override def toString: String = commandResult.toString
}

object SaneCommandPhrase {
  def apply(transfers: List[SaneTransferPhrase]): SaneCommandPhrase = {
    val commandTransfer = transfers.headOption err "Expected at least one transfer"
    val command = getCommandOrNone(commandTransfer) err s"Expected first transfer to be command: $commandTransfer"
    val otherTransfers = transfers.tail
    val commandResult = SaneCommandResult(command, commandTransfer, otherTransfers)
    SaneCommandPhrase(transfers, commandResult)
  }

  def isCommand(transfer: SaneTransferPhrase): Boolean = getCommandOrNone(transfer).isDefined

  def getCommandOrNone(transfer: SaneTransferPhrase): Option[Int] = {
    if (transfer.direction == UsbOut) {
      transfer.bytes match {
        case Array(0x1b, cmd) => Some(cmd & 0xff)
        case _ => None
      }
    } else {
      None
    }
  }
}

object SaneCommandPhraseDecoder extends PacketPhraseDecoder[SaneTransferPhrase, SaneCommandPhrase] {
  def decode(transfers: Stream[SaneTransferPhrase]): Stream[SaneCommandPhrase] = {
    // Split transfers at commands after sorting by host seqNo
    splitBefore(transfers sortBy (_.hostPacket.seqNo), SaneCommandPhrase.isCommand(_: SaneTransferPhrase)) map (SaneCommandPhrase(_))
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
