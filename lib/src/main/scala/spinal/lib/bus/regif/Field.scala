package spinal.lib.bus.regif

import spinal.core._

case class Field(name: String,
                 hardbit: Data,
                 section: Range,
                 accType: AccessType,
                 resetValue: Long,
                 readError: Boolean,
                 doc: String){

  def tailBitPos = section.max

}