package epjitsu

import java.nio.file.Paths

object Program extends App {
  val packets = PcapFile.load(Paths.get(args(0)))
  try {
    packets foreach (println(_))
  } finally {
    packets.close()
  }
}
