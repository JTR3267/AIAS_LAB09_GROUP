package acal_lab09.PipelinedCPU.DatapathModule.DatapathComponent

import chisel3._
import chisel3.util._

import acal_lab09.PipelinedCPU.opcode_map._
import acal_lab09.PipelinedCPU.alu_op_map._

class ALUIO extends Bundle{
  val src1    = Input(UInt(32.W))
  val src2    = Input(UInt(32.W))
  val ALUSel  = Input(UInt(15.W))
  val out  = Output(UInt(32.W))
}

class ALU extends Module{
  val io = IO(new ALUIO)

  io.out := 0.U
  switch(io.ALUSel){
    is(ADD   ){io.out := io.src1+io.src2}
    is(SLL   ){io.out := io.src1 << io.src2(4,0)}
    is(SLT   ){io.out := Mux(io.src1.asSInt < io.src2.asSInt,1.U,0.U)}
    is(SLTU  ){io.out := Mux(io.src1 < io.src2              ,1.U,0.U)}
    is(XOR   ){io.out := io.src1^io.src2}
    is(SRL   ){io.out := io.src1 >> io.src2(4,0)}
    is(OR    ){io.out := io.src1|io.src2}
    is(AND   ){io.out := io.src1&io.src2}
    is(SUB   ){io.out := io.src1-io.src2}
    is(SRA   ){io.out := (io.src1.asSInt >> io.src2(4,0)).asUInt}
    is(CLZ   ){
      io.out := 32.U
      for (i <- 0 until 31){
        when(io.src1(i)){
          io.out := 31.U - i.U
        }
      }
    }
    is(CTZ   ){
      io.out := 32.U
      for (i <- 31 to 0 by -1){
        when(io.src1(i)){
          io.out := i.U
        }
      }
    }
    is(CPOP  ){io.out := PopCount(io.src1)}
    is(ANDN  ){io.out := io.src1 & (~io.src2)}
    is(ORN   ){io.out := io.src1 | (~io.src2)}
    is(XNOR  ){io.out := ~(io.src1 ^ io.src2)}
    is(MIN   ){io.out := Mux(io.src1.asSInt < io.src2.asSInt,io.src1,io.src2)}
    is(MAX   ){io.out := Mux(io.src1.asSInt > io.src2.asSInt,io.src1,io.src2)}
    is(MINU  ){io.out := Mux(io.src1 < io.src2,io.src1,io.src2)}
    is(MAXU  ){io.out := Mux(io.src1 > io.src2,io.src1,io.src2)}
    is(SEXT_B){
      when(io.src1(7)){
        io.out := Cat(~(0.U(24.W)), io.src1(7, 0))
      }.otherwise{
        io.out := Cat(0.U(24.W), io.src1(7, 0))
      }
    }
    is(SEXT_H){
      when(io.src1(15)){
        io.out := Cat(~(0.U(16.W)), io.src1(15, 0))
      }.otherwise{
        io.out := Cat(0.U(16.W), io.src1(15, 0))
      }
    }
    is(BSET  ){io.out := io.src1 | (1.U << io.src2(4, 0))}
    is(BCLR  ){io.out := io.src1 & ~(1.U(32.W) << io.src2(4, 0))}
    is(BINV  ){io.out := io.src1 ^ (1.U << io.src2(4, 0))}
  }
}

