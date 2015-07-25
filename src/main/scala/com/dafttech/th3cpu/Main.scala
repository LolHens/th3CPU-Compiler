package com.dafttech.th3cpu

import com.dafttech.th3cpu.mnemonic.MnemonicParser

/**
 * Created by LolHens on 25.07.2015.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val parser = new MnemonicParser()
    parser.parseAll(parser.instructions,
      """const(gpr1,30)
        |jmp
        |mov(mem_bus,gpr0)
        |breq""".stripMargin).get.foreach((i) => println(Integer.toBinaryString(i & 0xFF)))
  }
}
