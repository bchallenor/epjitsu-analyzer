package epjitsu

import java.io._

object Program extends App {
  val currentDir = System.getProperty("user.dir")

  val inputDir = (args sliding 2) collectFirst { case Array("--in-dir", x) => x } getOrElse currentDir
  println(s"Input dir: $inputDir")

  val outputDir = (args sliding 2) collectFirst { case Array("--out-dir", x) => x } getOrElse inputDir
  println(s"Output dir: $outputDir")

  val unknownCommands = Analyzer.analyzePcapFiles(new File(inputDir), new File(outputDir))

  println(s"Unknown commands:")
  unknownCommands.toSeq sortBy (_.headerTransfer.value.commandCode) foreach (println(_))
}
