package epjitsu

import scala.collection.immutable.SortedSet

sealed trait SaneCommandResult

case class SaneHardwareStatusCommandResult(bytes: Vector[Byte]) extends SaneCommandResult

case class SaneRead43CommandResult(bytes: Vector[Byte]) extends SaneCommandResult

case class SaneLampSetCommandResult(commandResult: Int, lampState: Boolean, lampStateResult: Int) extends SaneCommandResult

case class SaneSendFineCal1CommandResult(commandResult: Int, calBytes: Array[Byte], calResult: Int) extends SaneCommandResult {
  override def toString: String = s"SaneSendFineCal1CommandResult($commandResult, ${calBytes.length}, $calResult)"
}

case class SaneSendFineCal2CommandResult(commandResult: Int, calBytes: Array[Byte], calResult: Int) extends SaneCommandResult {
  override def toString: String = s"SaneSendFineCal2CommandResult($commandResult, ${calBytes.length}, $calResult)"
}

case class SaneSendLutCommandResult(commandResult: Int, lutBytes: Array[Byte], lutResult: Int) extends SaneCommandResult {
  override def toString: String = s"SaneSendLutCommandResult($commandResult, ${lutBytes.length}, $lutResult)"
}

case class SaneSendCoarseCalCommandResult(commandResult: Int, calBytes: Vector[Byte], calResult: Int) extends SaneCommandResult

case class SaneScanD1CommandResult(commandResult: Int, windowBytes: Vector[Byte], windowResult: Int) extends SaneCommandResult

case class SaneScanD2CommandResult(commandResult: Int, bytes: Array[Byte]) extends SaneCommandResult {
  override def toString: String = s"SaneScanD2CommandResult($commandResult, ${bytes.length})"
}

case class SaneScanD3CommandResult(commandResult: Int, bytes: Array[Byte]) extends SaneCommandResult {
  override def toString: String = s"SaneScanD3CommandResult($commandResult, ${bytes.length})"
}

case class SaneIngestPaperCommandResult(commandResult: Int, paperState: Boolean, paperStateResult: Int) extends SaneCommandResult

case class SaneScanD6CommandResult(commandResult: Int, bytes: Array[Byte]) extends SaneCommandResult {
  override def toString: String = s"SaneScanD6CommandResult($commandResult, ${bytes.length})"
}

case class SaneUnknownCommandResult(command: Int, commandTransfer: UsbBulkTransfer, otherTransfers: SortedSet[UsbBulkTransfer]) extends SaneCommandResult {
  override def toString: String = f"0x$command%02x\n${otherTransfers mkString "\n"}"
}

object SaneCommandResult {
  def apply(command: Int, commandTransfer: UsbBulkTransfer, otherTransfers: SortedSet[UsbBulkTransfer]): SaneCommandResult = (command, otherTransfers.toList) match {
    case (0x33, List(t1)) => SaneHardwareStatusCommandResult(t1.bytes.toVector)
    case (0x43, List(t1)) => SaneRead43CommandResult(t1.bytes.toVector)
    case (0xb0, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xb2, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xb3, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xb4, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xb5, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xc3, t1 :: rest) => SaneSendFineCal1CommandResult(extractResult(t1), joinBytes(rest dropRight 1), extractResult(rest.last))
    case (0xc4, t1 :: rest) => SaneSendFineCal2CommandResult(extractResult(t1), joinBytes(rest dropRight 1), extractResult(rest.last))
    case (0xc5, t1 :: rest) => SaneSendLutCommandResult(extractResult(t1), joinBytes(rest dropRight 1), extractResult(rest.last))
    case (0xc6, List(t1, t2, t3)) => SaneSendCoarseCalCommandResult(extractResult(t1), t2.bytes.toVector, extractResult(t3))
    case (0xd0, List(t1, t2, t3)) => SaneLampSetCommandResult(extractResult(t1), extractBoolean(t2), extractResult(t3))
    case (0xd1, List(t1, t2, t3)) => SaneScanD1CommandResult(extractResult(t1), t2.bytes.toVector, extractResult(t3))
    case (0xd2, t1 :: rest) => SaneScanD2CommandResult(extractResult(t1), joinBytes(rest))
    case (0xd3, t1 :: rest) => SaneScanD3CommandResult(extractResult(t1), joinBytes(rest))
    case (0xd4, List(t1, t2, t3)) => SaneIngestPaperCommandResult(extractResult(t1), extractBoolean(t2), extractResult(t3))
    case (0xd6, t1 :: rest) => SaneScanD6CommandResult(extractResult(t1), joinBytes(rest))
    case (0xd8, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
    case (0xe1, _) => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)

    case _ => SaneUnknownCommandResult(command, commandTransfer, otherTransfers)
  }

  private def extractResult(transfer: UsbBulkTransfer): Int = {
    transfer.bytes match {
      case Array(x) => x & 0xff
      case other => sys.error(s"Expected single byte: ${transfer.bytes.toVector}")
    }
  }

  private def extractBoolean(transfer: UsbBulkTransfer): Boolean = {
    transfer.bytes match {
      case Array(0x00) => false
      case Array(0x01) => true
      case other => sys.error(s"Expected boolean: ${transfer.bytes.toVector}")
    }
  }

  private def joinBytes(transfers: List[UsbBulkTransfer]): Array[Byte] = {
    transfers.toArray flatMap (_.bytes)
  }
}
