package epjitsu

import scala.collection.breakOut

case class ScanConfig(mode: Mode.Value, inRes: InRes.Value, plexity: Plexity.Value, finishAction: FinishAction.Value) {
  val exRes = ExRes(inRes, mode)
  val fileNameNoExt = s"$mode-$exRes-$plexity-$finishAction"
}

object ScanConfig {
  val values: Set[ScanConfig] = (for {
    mode <- Mode.values
    inRes <- InRes.values
    plexity <- Plexity.values
    finishAction <- FinishAction.values
  } yield ScanConfig(mode, inRes, plexity, finishAction)).toSet

  private val fileNameNoExtToScanConfig: Map[String, ScanConfig] = values.map(x => (x.fileNameNoExt, x))(breakOut)

  def unapply(fileNameNoExt: String): Option[ScanConfig] = fileNameNoExtToScanConfig.get(fileNameNoExt)
}

object Mode extends Enumeration {
  val BlackWhite = Value("bw")
  val Gray = Value("gray")
  val Color = Value("color")
}

object InRes extends Enumeration {
  val In150 = Value("150")
  val In225 = Value("225")
  val In300 = Value("300")
  val In600 = Value("600")
}

object ExRes extends Enumeration {
  val Ex150 = Value("150")
  val Ex200 = Value("200")
  val Ex300 = Value("300")
  val Ex400 = Value("400")
  val Ex600 = Value("600")
  val Ex1200 = Value("1200")

  def apply(inRes: InRes.Value, mode: Mode.Value): ExRes.Value = {
    import InRes._
    import Mode._
    (inRes, mode) match {
      case (In150, Gray | Color) => Ex150
      case (In225, Gray | Color) => Ex200
      case (In300, Gray | Color) => Ex300
      case (In600, Gray | Color) => Ex600
      case (In150, BlackWhite  ) => Ex300
      case (In225, BlackWhite  ) => Ex400
      case (In300, BlackWhite  ) => Ex600
      case (In600, BlackWhite  ) => Ex1200
    }
  }
}

object Plexity extends Enumeration {
  type Plexity = Value
  val Simplex = Value("simplex")
  val Duplex = Value("duplex")
}

object FinishAction extends Enumeration {
  type FinishAction = Value
  val Stop = Value("stop")
  val Continue = Value("cont")
}
