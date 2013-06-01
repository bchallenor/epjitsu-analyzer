package epjitsu.util

class BytesPrettyPrinter(bytes: Array[Byte]) {
  def prettyPrint(maxSize: Int = 128) = if (bytes.size <= maxSize) bytes map ("0x%02x" format _) mkString("{", ", ", "}") else s"${bytes.size} bytes"
}

object BytesPrettyPrinter {
  implicit def bytesPrettyPrinter(bytes: Array[Byte]) = new BytesPrettyPrinter(bytes)
}
