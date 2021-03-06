
package spinal.tester.code



import spinal.core.Nameable.{DATAMODEL_WEAK, USER_WEAK}
import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

import scala.collection.mutable.ArrayBuffer


object Debug {
  class NameVecIndexing extends Component {
    val io = new Bundle {
      val qpsize = in  UInt(32 bits)
      val addr1 = in  UInt(32 bits)
      val addr2 = in  UInt(32 bits)
      val addr3 = in  UInt(32 bits)
      val qlevel = out (UInt(32 bits))
    }
    noIoPrefix()

    val qp_f = Vec(Vec(Vec(UInt(32 bits), 5), 4), 3)

    for (i <- 0 until 3) (
      for (j <- 0 until 4) (
        for (k <- 0 until 5) {
          qp_f(i)(j)(k) := i + j + k
        }
        )
      )

    io.addr1.addTag(tagAutoResize)
    io.addr2.addTag(tagAutoResize)
    io.addr3.addTag(tagAutoResize)
    io.qlevel := qp_f(io.addr1(8 downto 4).addTag(tagAutoResize))(io.addr2)(io.addr3)
  }


  def main(args: Array[String]) {
    SpinalVerilog(new NameVecIndexing)

  }

  //  createEnum("asd")
}


object Debug2 extends App{

//  class Miaou extends Bundle{
//
//
//    def fifo2() = new Composite{
//      val x = 4
//    }.x
//
//    def fifo1(): Unit = new Area {
//      val x = UInt(8 bits)
//
//      this.setCompositeName(Miaou.this)
//      x + 1
//    }
////    def fifo(): Unit = composite(this) {
////      val x = UInt(8 bits)
////
////      x + 1
////    }
//  }


  SpinalConfig().includeFormal.generateSystemVerilog(new Component{
    val rawrrr = in UInt(8 bits)
    val wuff = out(Reg(UInt(8 bits))) init(0x11)
    wuff := wuff + rawrrr



//    GenerationFlags.formal {
//      when(Formal.initstate()) {
//        assume(clockDomain.isResetActive)
//      }
//    }


    GenerationFlags.formal {
      ClockDomain.current.withoutReset(){
        assert(wuff === 0)
      }
//      ClockDomain.current.readResetWire initial(False)
//      rawrrr.initial(0x42)
//
//      assumeInitial(!clockDomain.isResetActive)
//      ClockDomain.current.duringReset {
//        assume(rawrrr === 0)
//        assume(wuff === 3)
//      }
    }

    setDefinitionName("miaou")
  })

}

//object MyEnum extends  spinal.core.MacroTest.mkObject("asd")
//
//



import spinal.core._

class Test extends Component {
  val io = new Bundle {
    val input = in Vec(UInt(10 bits), 2)
    val output = out UInt(11 bits)
  }

  io.output := io.input(0) +^ io.input(1)

}

class TestTop extends Component {
  val io = new Bundle {
    val A = in UInt(10 bits)
    val B = in UInt(10 bits)
    val C = out UInt(11 bits)
  }

  val adder = new Test
  adder.io.input(0) := io.A
  adder.io.input(1) := io.B
  io.C := adder.io.output
}

object TestTopMain {
  def main(args: Array[String]) {
    SpinalVerilog(new TestTop)
  }
}




object Debug3 extends App{
  import spinal.core.sim._

  SimConfig.withIVerilog.compile(new Component {
    val a,b,c = in UInt(8 bits)
    val result = out(Reg(UInt(8 bits)) init(0))
    result := result + 1
    assert(result =/= 10, "miaou")
  }).doSim{ dut =>
    dut.a #= 1
    dut.b #= 0
    dut.c #= 0
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(100)
  }
}


object Debug4 extends App{
  SpinalVerilog(new Component{
    val mem = Mem(UInt(8 bits), 16) initBigInt(List.fill(16)(BigInt(0)))

    val a = slave(mem.writePort)
    val b = slave(mem.readSyncPort)
    println(LatencyAnalysis(a.data, b.rsp))
    println(LatencyAnalysis(b.cmd.payload, b.rsp))
  })
}
