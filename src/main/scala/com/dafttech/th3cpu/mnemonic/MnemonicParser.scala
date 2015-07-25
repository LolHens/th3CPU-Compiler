package com.dafttech.th3cpu.mnemonic

import java.nio.file.{Files, Paths}

import org.lolhens.parser.ParserUtils

/**
 * Created by LolHens on 25.07.2015.
 */
object MnemonicParser extends ParserUtils {
  def main(args: Array[String]): Unit = {
    val inName = args.mkString(" ")
    val outName = (if (inName.contains(".")) inName.take(inName.lastIndexOf(".")) else inName) + ".bin"

    Files.write(Paths.get(outName), parseAll(instructions, Files.newBufferedReader(Paths.get(inName))).get.toArray)
  }

  def instructions: Parser[List[Byte]] = rep(instruction) ^^ (_.flatten)

  def instruction: Parser[List[Byte]] = const ~ opt(branch) ^^ {
    case const ~ None => const
    case const ~ Some(branch) => List((const(0) | branch).toByte, const(1))
  } | move ~ opt(branch) ^^ {
    case move ~ None => List(move)
    case move ~ Some(branch) => List((move | branch).toByte)
  } | branch ^^ (List(_))

  private def move: Parser[Byte] = ("mv" | "mov" | "move") ~> optFrame("(", writeRegister ~ "," ~ readRegister, ")") ^^ {
    case target ~ _ ~ source => ((target << 3) | source).toByte
  }

  private val paramRegister = 4

  private def const: Parser[List[Byte]] = "const" ~> optFrame("(", writeRegister ~ "," ~ wholeNumber, ")") ^^ {
    case target ~ _ ~ const => List(((target << 3) | paramRegister).toByte, const.toByte)
  }

  private def branch: Parser[Byte] = (("jmp" | "jump") | "breq" | "brne") ^^ ((jmp) => {
    val byte = jmp match {
      case "jmp" | "jump" => 1
      case "breq" => 2
      case "brne" => 3
      case _ => 0
    }
    (byte << 6).toByte
  })

  private def writeRegister: Parser[Byte] = (
    byteType |
      "gpr0" |
      "gpr1" |
      "gpr2" |
      "gpr3" |
      "ds" |
      "ptr" |
      "cs" |
      ("mem_bus" | "membus")
    ) ^^ {
    case index: Byte => index
    case "gpr0" => 0
    case "gpr1" => 1
    case "gpr2" => 2
    case "gpr3" => 3
    case "ds" => 4
    case "ptr" => 5
    case "cs" => 6
    case "mem_bus" | "membus" => 7
  }


  private def readRegister: Parser[Byte] = (
    byteType |
      "gpr0" |
      "gpr1" |
      "gpr2" |
      "gpr3" |
      ("mem_bus" | "membus")
    ) ^^ {
    case index: Byte => index
    case "gpr0" => 0
    case "gpr1" => 1
    case "gpr2" => 2
    case "gpr3" => 3
    case "mem_bus" | "membus" => 7
  }
}
