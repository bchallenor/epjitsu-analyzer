package epjitsu

import epjitsu.util.PrettyPrint.BytesPrettyPrint
import epjitsu.util.DeepByteArray
import org.joda.time.DateTime

case class UsbTransfer(seqNo: Long, timestamp: DateTime, requestId: Long, xferType: UsbTransferType, direction: TransferDir, bus: Int, device: Int, endpoint: Int, bytes: Array[Byte]) extends Transfer {
  def address: (Int, Int) = (bus, device)

  private def directionStr: String = direction match {
    case OutDir => "-->"
    case InDir => "<--"
  }

  override def toString: String = f"#$seqNo $address $directionStr $address ${BytesPrettyPrint.prettyPrint(DeepByteArray(bytes))}"
}
