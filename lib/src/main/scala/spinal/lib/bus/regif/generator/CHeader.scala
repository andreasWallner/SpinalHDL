package spinal.lib.bus.regif.generator

import spinal.lib.bus.regif._
import spinal.core.{GlobalData, widthOf}
import java.io.{Writer, PrintWriter, File}

// TODO register prefix
// why not a visitor: https://stackoverflow.com/questions/8618082/visitor-pattern-in-scala

class CHeader(intf: BusIf) {
  private var name = intf.getModuleName
  
  def overrideName(newName: String) = {
    name = newName
    this
  }

  def write(): Unit = write(f"${GlobalData.get.phaseContext.config.targetDirectory}/${name}.h")

  def write(filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
    try {
      write(writer)
    } finally {
      writer.close();
    }
  }

  def write(writer: Writer): Unit = {
    writer.write(s"""
      |#ifndef header_${name}_h
      |#define header_${name}_h
      |#include <stdint.h>
      |
      |#if __cplusplus
      |extern "C" {
      |#endif
      |
      |""".stripMargin)

    writeRegisterOffsets(intf, writer)
    writeBankStruct(intf, writer)
    writeRegisterStructs(intf, writer)
    writeFieldInfos(intf, writer)

    writer.write("""
      |#if __cplusplus
      |}
      |#endif
      |#endif
      |""".stripMargin)
  }

  def writeRegisterOffsets(intf: BusIf, writer: Writer): Unit = {
    intf.registers.foreach(reg => {writer.write(f"#define ${name.toUpperCase()}_${reg.name.toUpperCase()}_OFFSET 0x${reg.addr}%04x\n")}) // TODO use correct literal macro/postfix
    writer.write("\n")
  }

  def writeBankStruct(intf: BusIf, writer: Writer): Unit = {
    val baseType = s"uint${intf.busDataWidth}_t"
    writer.write("struct {\n")
    intf.registers.foldLeft(0L) {(expectedAddress, reg) => {
      if (reg.addr != expectedAddress) {
        writer.write(s"  ${baseType} rfu_${reg.addr}[${(expectedAddress - reg.addr) / intf.busDataWidth};\n")
      }
      writer.write(s"  ${baseType} ${reg.name.toUpperCase()};\n")

      reg.addr + intf.wordAddressInc
    }}
    writer.write(s"} ${name.toLowerCase()}_register_t;\n\n")
  }

  def writeRegisterStructs(intf: BusIf, writer: Writer): Unit = intf.registers.foreach(writeRegisterStruct(_, writer))

  def writeRegisterStruct(reg: RegInst, writer: Writer): Unit = {
    val baseType = s"uint${reg.busif.busDataWidth}_t"
    writer.write(s"typedef union {\n")
    writer.write(s"  ${baseType} v;\n")
    writer.write(s" struct {\n")
    reg.fieldss.foreach(field => {
      val name = field.accType match {
        case AccessType.NA => f"rfu_${field.section.head}"
        case _ => field.name
      }
      writer.write(s"    ${baseType} ${name}:${widthOf(field.hardbit)};\n")
    })
    writer.write(s"  } b;\n")
    writer.write(s"} ${reg.name.toLowerCase()}_t;\n\n")
  }

  def bitmask(range: Range): Long = {
    val ones = java.lang.Long.MAX_VALUE
    val rightCleared = ones << range.end
    rightCleared & ~(ones << (range.start + 1))
  }

  def writeFieldInfos(intf: BusIf, writer: Writer): Unit = {
    for(register <- intf.registers) {
      for(field <- register.fieldss if field.accType != AccessType.NA) {
        val define = s"${name}_${register.name}_${field.name}".toUpperCase
        writer.write(s"#define ${define}_Pos ${field.section.end}\n")
        writer.write(f"#define ${define}_Msk 0x${bitmask(field.section)}%04x\n") // TODO use correct literal macro/postfix
      }
    }
    writer.write("\n")
  }
}