package epjitsu

import scalaz._
import Scalaz._

case class SaneCommandPhrase(transfers: List[SaneTransferPhrase]) extends PacketPhrase[SaneTransferPhrase] {
  def packets = transfers

  val commandTransfer: SaneTransferPhrase = transfers.headOption err "Expected at least one transfer"
  val command: Byte = SaneCommandPhraseDecoder.getCommandOrNone(commandTransfer) err s"Expected first transfer to be command: $commandTransfer"

  val otherTransfers: List[SaneTransferPhrase] = transfers.tail

  override def toString: String = f"0x$command%02x\n${transfers mkString "\n"}"
}

object SaneCommandPhraseDecoder extends PacketPhraseDecoder[SaneTransferPhrase, SaneCommandPhrase] {
  def decode(transfers: Stream[SaneTransferPhrase]): Stream[SaneCommandPhrase] = {
    // Split transfers at commands after sorting by host seqNo
    splitBefore(transfers sortBy (_.hostPacket.seqNo), isCommand(_: SaneTransferPhrase)) map SaneCommandPhrase
  }

  def isCommand(transfer: SaneTransferPhrase): Boolean = getCommandOrNone(transfer).isDefined

  def getCommandOrNone(transfer: SaneTransferPhrase): Option[Byte] = {
    if (transfer.direction == UsbOut) {
      transfer.bytes match {
        case Array(0x1b, cmd) => Some(cmd)
        case _ => None
      }
    } else {
      None
    }
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
