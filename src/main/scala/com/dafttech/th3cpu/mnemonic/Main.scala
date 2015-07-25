package com.dafttech.th3cpu.mnemonic

import java.nio.file.{Files, Paths}

/**
 * Created by LolHens on 25.07.2015.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val inName = args.mkString(" ")
    val outName = (if (inName.contains(".")) inName.take(inName.lastIndexOf(".")) else inName) + ".bin"

    val parser = new MnemonicParser

    Files.write(Paths.get(outName), parser.parseAll(parser.instructions, Files.newBufferedReader(Paths.get(inName))).get.toArray)
  }
}
