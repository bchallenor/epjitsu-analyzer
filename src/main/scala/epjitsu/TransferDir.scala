package epjitsu

sealed trait TransferDir
case object InDir extends TransferDir
case object OutDir extends TransferDir
