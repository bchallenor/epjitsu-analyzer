package epjitsu

import java.io.DataInput

trait Payload

trait PayloadDecoder {
  def decode(dataInput: DataInput): Payload
}
