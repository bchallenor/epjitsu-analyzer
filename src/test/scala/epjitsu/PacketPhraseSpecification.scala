package epjitsu

import org.scalacheck.Properties
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties

object PacketPhraseSpecification extends Properties("PacketPhrase") {
  // todo: implement and check the PacketPhrase type classes
  include(ScalazProperties.monad.laws[Option])
}
