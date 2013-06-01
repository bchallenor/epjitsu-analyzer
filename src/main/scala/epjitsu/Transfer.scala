package epjitsu

import epjitsu.util.PrettyPrint

trait Transfer extends Packet {
  def direction: TransferDir
  def bytes: Array[Byte]
}

object Transfer {
  type TransferPhrase[A] = PacketPhrase[Transfer, A]

  implicit object TransferPrettyPrint extends PrettyPrint[Transfer] {
    override def prettyPrint(value: Transfer) = value.toString
  }
}
