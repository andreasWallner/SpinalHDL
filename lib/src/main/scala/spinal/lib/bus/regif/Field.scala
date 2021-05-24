package spinal.lib.bus.regif

import spinal.core._

case class KnownValue(value: Long, name: String, doc: Option[String]) {}

case class Field(
    name: String,
    hardbit: Data,
    section: Range,
    accType: AccessType,
    resetValue: Long,
    readError: Boolean,
    doc: String,
    knownValues: List[KnownValue]) {

  def tailBitPos = section.max
}