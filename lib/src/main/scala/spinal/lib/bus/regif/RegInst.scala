package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import scala.collection.mutable.ListBuffer

class Section(val max: Int, val min: Int) {
  override def toString(): String = {
    if(this.max == this.min) {
      s"[${this.min}]"
    } else {
      s"[${this.max}:${this.min}]"
    }
  }
}

object Section{
  def apply(x: Range): Section = new Section(x.max, x.min)
  implicit def tans(x: Range) = Section(x)
}


case class RamInst(name: String, sizeMap: SizeMapping, busif: BusIf) {
  private var Rerror: Boolean = false
  def readErrorTag = Rerror

  def hitRange(addr: UInt): Bool = {
    val hit = False
    when(addr >= sizeMap.base && addr < (sizeMap.base + sizeMap.size)){
      hit := True
    }
    hit
  }

  val hitRead  = hitRange(busif.readAddress)
  val hitWrite = hitRange(busif.writeAddress)
  val hitDoRead  = hitRead && busif.doRead
  val hitDoWrite = hitWrite && busif.doWrite

}

class FIFOInst(name: String, addr: Long, doc:String, busif: BusIf) extends RegBase(name,addr,doc,busif) {

}

case class DataFieldBuilder[T <: Data](
    register: RegInst,
    dataType: HardType[T],
    acc: AccessType,
    name: String,
    reset: Option[Long] = None,
    docString: Option[String] = None,
    alignment: Option[Integer] = None,
    knownValues: List[KnownValue] = List())
{
  def apply(): T = {
    register.makeField(this)
  }
  def init(that: Long): DataFieldBuilder[T] = this.copy(reset=Some(that))
  def doc(doc: String): DataFieldBuilder[T] = this.copy(docString=Some(doc))
  def align(multiple: Integer): DataFieldBuilder[T] = this.copy(alignment=Some(multiple))
  def value(that: Long, name: String): DataFieldBuilder[T] = value(that, name, None)
  def value(that: Long, name: String, doc: String): DataFieldBuilder[T] = value(that, name, Some(doc))
  // TODO
  //def value(that: T, name: String) = value(that.asL)
  //def value(that: T, name: String, doc: String)
  private def value(that: Long, name: String, doc: Option[String]): DataFieldBuilder[T] = this.copy(knownValues = (knownValues ++ List(KnownValue(that, name, doc))))
}

case class RegInst(name: String, addr: Long, doc: String, busif: BusIf) extends RegBase(name, addr, doc, busif) {
  def checkLast={
    val spareNumbers = if(fields.isEmpty) busif.busDataWidth else busif.busDataWidth-1 - fields.last.tailBitPos
    spareNumbers match {
      case x if x > 0 => field(x bits, AccessType.NA)(SymbolName("reserved"))
      case x if x < 0 => SpinalError(s"Range ${Section(fields.last.section)} exceed Bus width ${busif.busDataWidth}")
      case _ =>
    }
  }

  def allIsNA: Boolean = {
    checkLast
    fields.map(_.accType == AccessType.NA).foldLeft(true)(_&&_)
  }

  def fieldAt(pos: Int, bc: BitCount, acc: AccessType, resetValue: Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = {
    val sectionNext: Section = pos+bc.value-1 downto pos
    val sectionExists: Section = fieldPtr downto 0
    val ret = pos match {
      case x if x < fieldPtr => SpinalError(s"field Start Point ${x} conflict to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= busif.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${busif.busDataWidth}")
      case x if (x == fieldPtr) => field(bc, acc, resetValue, doc)
      case _ => {
        field(pos - fieldPtr bits, AccessType.NA)(SymbolName("reserved"))
        field(bc, acc, resetValue, doc)
      }
    }
    fieldPtr = pos + bc.value
    ret
  }

  def typedField[T <: Data](dataType: T, acc: AccessType)(implicit symbol: SymbolName): DataFieldBuilder[T] =
    DataFieldBuilder(this, dataType, acc, symbol.name)

  def makeField[T <: Data](builder: DataFieldBuilder[T]): T =
    typed(builder.dataType, builder.acc, builder.reset.getOrElse(0), builder.docString.getOrElse(""), builder.name, builder.alignment.getOrElse(1), builder.knownValues)

  def typed[T <: Data](dataType: HardType[T], acc: AccessType, resetValue: Long, doc: String, name: String, alignment: Integer = 1, knownValues: List[KnownValue] = List()): T = {
    if (acc == AccessType.RO && resetValue != 0)
      SpinalError("RO field can't have a reset value")

    val width = dataType.getBitsWidth
    val alignedFieldPtr = fieldPtr + ((4 - (fieldPtr % alignment)) % alignment) 
    print(f"${name} ${alignedFieldPtr} ${alignment}\n")
    if (alignedFieldPtr != fieldPtr)
      typed(Bits((alignedFieldPtr - fieldPtr) bits), AccessType.NA, 0, "", "")
    val section: Range = (alignedFieldPtr + width - 1) downto alignedFieldPtr
    val ret : T = acc match {
      case AccessType.RO => RO(dataType)
      case AccessType.RW    => W(dataType, section, resetValue)  //- W: as-is, R: no effect
      case AccessType.RC    => RC(dataType, resetValue)           //- W: no effect, R: clears all bits
      case AccessType.RS    => RS(dataType, resetValue)           //- W: no effect, R: sets all bits
      case AccessType.WRC   => WRC(dataType, section, resetValue) //- W: as-is, R: clears all bits
      case AccessType.WRS   => WRS(dataType, section, resetValue) //- W: as-is, R: sets all bits
      case AccessType.WC    => WC(dataType, resetValue)           //- W: clears all bits, R: no effect
      case AccessType.WS    => WS(dataType, resetValue)           //- W: sets all bits, R: no effect
      case AccessType.WSRC  => WSRC(dataType, resetValue)         //- W: sets all bits, R: clears all bits
      case AccessType.WCRS  => WCRS(dataType, resetValue)         //- W: clears all bits, R: sets all bits
      case AccessType.W1C   => WB(dataType, section, resetValue, AccessType.W1C )   //- W: 1/0 clears/no effect on matching bit, R: no effect
      case AccessType.W1S   => WB(dataType, section, resetValue, AccessType.W1S )   //- W: 1/0 sets/no effect on matching bit, R: no effect
      case AccessType.W1T   => WB(dataType, section, resetValue, AccessType.W1T )   //- W: 1/0 toggles/no effect on matching bit, R: no effect
      case AccessType.W0C   => WB(dataType, section, resetValue, AccessType.W0C )   //- W: 1/0 no effect on/clears matching bit, R: no effect
      case AccessType.W0S   => WB(dataType, section, resetValue, AccessType.W0S )   //- W: 1/0 no effect on/sets matching bit, R: no effect
      case AccessType.W0T   => WB(dataType, section, resetValue, AccessType.W0T )   //- W: 1/0 no effect on/toggles matching bit, R: no effect
      case AccessType.W1SRC => WBR(dataType, section, resetValue, AccessType.W1SRC) //- W: 1/0 sets/no effect on matching bit, R: clears all bits
      case AccessType.W1CRS => WBR(dataType, section, resetValue, AccessType.W1CRS) //- W: 1/0 clears/no effect on matching bit, R: sets all bits
      case AccessType.W0SRC => WBR(dataType, section, resetValue, AccessType.W0SRC) //- W: 1/0 no effect on/sets matching bit, R: clears all bits
      case AccessType.W0CRS => WBR(dataType, section, resetValue, AccessType.W0CRS) //- W: 1/0 no effect on/clears matching bit, R: sets all bits
      case AccessType.WO    => Rerror = true; W(dataType, section, resetValue) //- W: as-is, R: error
      case AccessType.WOC   => Rerror = true; WC(dataType, resetValue)          //- W: clears all bits, R: error
      case AccessType.WOS   => Rerror = true; WS(dataType, resetValue)          //- W: sets all bits, R: error
      case AccessType.W1    =>                W1(dataType, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: no effect
      case AccessType.WO1   => Rerror = true; W1(dataType, section, resetValue) //- W: first one after ~HARD~ reset is as-is, other W have no effects, R: error
      case AccessType.NA    => NA(dataType)                                     // -W: reserved, R: reserved
      case AccessType.W1P   => WBP(dataType, section, resetValue, AccessType.W1P )  //- W: 1/0 pulse/no effect on matching bit, R: no effect
      case AccessType.W0P   => WBP(dataType, section, resetValue, AccessType.W0P )  //- W: 0/1 pulse/no effect on matching bit, R: no effect
    }
    val newdoc = if(doc.isEmpty && acc == AccessType.NA) "Reserved" else doc
    val nameRemoveNA = if(acc == AccessType.NA) "--" else name
    checkSectionIsValid(section)
    fields   += Field(nameRemoveNA, ret, section, acc, 0, Rerror, newdoc, knownValues)
    fieldPtr += width

    ret
  }

  def checkSectionIsValid(section: Section) = {
    if (section.min < 0)
      SpinalError("invalid section with min < 0")
    if (section.max > busif.busDataWidth)
      SpinalError(f"invalid section: end out of range, register is ${busif.busDataWidth} bits wide, ${section.max} bits would be needed")
    val overlapping = fields.filter(other => (section.max < other.section.min && other.section.max > section.min))
    if(!overlapping.isEmpty) {
      val other = overlapping.head
      SpinalError(f"invalid section: ${section} overlaps with existing field ${other.name} at ${other.section}")
    }
  }

  def field(bc: BitCount, acc: AccessType, resetValue:Long = 0, doc: String = "")(implicit symbol: SymbolName): Bits = typed(Bits(bc), acc, resetValue, doc, symbol.name)

  def reserved(bc: BitCount): Bits = field(bc, AccessType.NA)(SymbolName("reserved"))

  // RegDescr implementation
  def getName()        : String           = name
  def getAddr()        : Long             = addr
  def getDoc()         : String           = doc
  //def getFieldDescrs() : List[FieldDescr] = getFields

  //def accept(vs : BusIfVisitor) = {
  //    vs.visit(this)
  //}
}

abstract class RegBase(name: String, addr: Long, doc: String, busif: BusIf) {
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

  def readErrorTag = Rerror
  def getFields = fields.toList

  val hitRead  = busif.readAddress === U(addr)
  val hitWrite = busif.writeAddress === U(addr)
  val hitDoRead  = hitRead && busif.doRead
  val hitDoWrite = hitWrite && busif.doWrite

  def readBits: Bits = {
    fields.map(_.hardbit).reverse.foldRight(Bits(0 bit))((x,y) => x ## y) //TODO
  }

  def eventR() : Bool = {
    val event = Reg(Bool) init(False)
    event := hitDoRead
    event
  }

  def eventW() : Bool = {
    val event = Reg(Bool) init(False)
    event := hitDoWrite
    event
  }

  protected def RO[T <: Data](dataType: HardType[T]): T = dataType()

  protected def W1[T <: Data](dataType: HardType[T], section: Range, resetValue: Long ): T ={
    val ret = Reg(dataType()) init B(resetValue).as(dataType)
    val hardRestFirstFlag = Reg(Bool()) init True
    when(hitDoWrite && hardRestFirstFlag){
      ret := busif.writeData(section).as(dataType)
      hardRestFirstFlag.clear()
    }
    ret
  }

  protected def W[T <: Data](dataType: HardType[T], section: Range, resetValue: Long ): T ={
    val ret = Reg(dataType) init B(resetValue, dataType.getBitsWidth bit).as(dataType)
    when(hitDoWrite){
      ret := busif.writeData(section).as(dataType)
    }
    ret
  }

  protected def clearAll[T <: Data](that: T): T = {
    for(i <- 0 until widthOf(that)){
      that.assignFromBits(B"0", i, 1 bits)
    }
    that
  }

  protected def setAll[T <: Data](that: T): T = {
    for(i <- 0 until widthOf(that)){
      that.assignFromBits(B"1", i, 1 bits)
    }
    that
  }

  protected def RC[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoRead) {
      clearAll(ret)
    }
    ret
  }

  protected def RS[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      setAll(ret)
    }
    ret
  }

  protected def WRC[T <: Data](dataType: HardType[T], section: Range, resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      ret := busif.writeData(section).as(dataType)
    }.elsewhen(hitDoRead){
      clearAll(ret)
    }
    ret
  }

  protected def WRS[T <: Data](dataType: HardType[T], section: Range, resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      ret := busif.writeData(section).as(dataType)
    }.elsewhen(hitDoRead){
      setAll(ret)
    }
    ret
  }

  protected def WC[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue, dataType.getBitsWidth bit).as(dataType)
    when(hitDoWrite){
      clearAll(ret)
    }
    ret
  }

  protected def WS[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      setAll(ret)
    }
    ret
  }

  protected def WSRC[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      setAll(ret)
    }.elsewhen(hitDoRead){
      clearAll(ret)
    }
    ret
  }

  protected def WCRS[T <: Data](dataType: HardType[T], resetValue: Long): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      clearAll(ret)
    }.elsewhen(hitDoRead){
      setAll(ret)
    }
    ret
  }

  protected def WB[T <: Data](dataType: HardType[T], section: Range, resetValue: Long, accType: AccessType): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    when(hitDoWrite){
      for(x <- section) {
        val idx = x - section.min
        accType match {
          case AccessType.W1C => when( busif.writeData(x)){ ret.assignFromBits(B"0", idx, 1 bits) }
          case AccessType.W1S => when( busif.writeData(x)){ ret.assignFromBits(B"1", idx, 1 bits) }
          case AccessType.W1T => when( busif.writeData(x)){ ret.assignFromBits((~ret.asBits(idx)).asBits, idx, 1 bits) }
          case AccessType.W0C => when(~busif.writeData(x)){ ret.assignFromBits(B"0", idx, 1 bits) }
          case AccessType.W0S => when(~busif.writeData(x)){ ret.assignFromBits(B"1", idx, 1 bits) }
          case AccessType.W0T => when(~busif.writeData(x)){ ret.assignFromBits((~ret.asBits(idx)).asBits, idx, 1 bits) }
          case _ =>
        }
      }
    }
    ret
  }

  protected def WBR[T <: Data](dataType: HardType[T], section: Range, resetValue: Long, accType: AccessType): T = {
    val ret = Reg(dataType) init B(resetValue).as(dataType)
    for(x <- section) {
      val idx = x - section.min
      accType match {
        case AccessType.W1SRC => {
          when(hitDoWrite && busif.writeData(x)) { ret.assignFromBits(B"1", idx, 1 bits) }
            .elsewhen(hitDoRead)                 { ret.assignFromBits(B"0", idx, 1 bits) }
        }
        case AccessType.W1CRS => {
          when(hitDoWrite && busif.writeData(x)) { ret.assignFromBits(B"1", idx, 1 bits) }
            .elsewhen(hitDoRead)                 { ret.assignFromBits(B"0", idx, 1 bits) }
        }
        case AccessType.W0SRC => {
          when(hitDoWrite && ~busif.writeData(x)){ ret.assignFromBits(B"0", idx, 1 bits) }
            .elsewhen(hitDoRead)                 { ret.assignFromBits(B"1", idx, 1 bits) }
        }
        case AccessType.W0CRS => {
          when(hitDoWrite && ~busif.writeData(x)){ ret.assignFromBits(B"1", idx, 1 bits) }
            .elsewhen(hitDoRead)                 { ret.assignFromBits(B"0", idx, 1 bits) }
        }
        case _ =>
      }
    }
    ret
  }

  protected def WBP[T <: Data](dataType: HardType[T], section: Range, resetValue: Long, accType: AccessType): T = {
    val resetValues = B(resetValue, dataType.getBitsWidth bit)
    val ret = Reg(dataType) init resetValues.as(dataType)
    for(x <- section) {
      val idx = x - section.min
      accType match {
        case AccessType.W1P => {
          when(hitDoWrite &&  busif.writeData(x)){ ret.assignFromBits((~ret.asBits(idx)).asBits, idx, 1 bits) }
            .otherwise{ ret.assignFromBits(B"0", idx, 1 bits) }
        }
        case AccessType.W0P => {
          when(hitDoWrite && ~busif.writeData(x)){ ret.assignFromBits((~ret.asBits(idx)).asBits, idx, 1 bits) }
            .otherwise{ ret.assignFromBits(resetValues.asBits(idx to idx), idx, 1 bits) }
        }
        case _ =>
      }
    }
    ret
  }

  protected def NA[T <: Data](dataType: HardType[T]): T = {
    clearAll(dataType())
  }
}
