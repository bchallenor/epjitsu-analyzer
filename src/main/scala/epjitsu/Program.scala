package epjitsu

import java.io._
import scalaz._
import Scalaz._

object Program extends App {
  val currentDir = new File(System.getProperty("user.dir"))

  val inputDir = args sliding 2 collectFirst { case Array("--in-dir", x) => new File(x) } getOrElse currentDir
  println(s"Input dir: $inputDir")

  val outputDir = args sliding 2 collectFirst { case Array("--out-dir", x) => new File(x) } getOrElse inputDir
  println(s"Output dir: $outputDir")

  println()

  val pcapFiles = inputDir.listFiles(new FilenameFilter {
    def accept(dir: File, name: String): Boolean = name.endsWith(".pcap")
  }).toList

  val unknownCommands = (pcapFiles map { pcapFile =>
    assert(pcapFile.getName.endsWith(".pcap"))
    val fileNameNoExt = pcapFile.getName.dropRight(".pcap".length)

    val commands = Analyzer.analyzePcapFile(pcapFile)

    Analyzer.logCommands(commands, new File(outputDir, pcapFile.getName + ".log"))

    fileNameNoExt match {
      case ScanConfig(scanConfig) =>
        val inRes = scanConfig.inRes
        Analyzer.logHeaderMagic(commands, new File(outputDir, inRes.toString + "@" + pcapFile.getName + ".h"), inRes)
      case _ =>
    }

    val unknownCommands = Analyzer.collectUnknownCommands(commands)

    println()
    unknownCommands
  }).concatenate

  Analyzer.logUnknownCommands(unknownCommands, new File(outputDir, "unknown-commands.log"))
}


