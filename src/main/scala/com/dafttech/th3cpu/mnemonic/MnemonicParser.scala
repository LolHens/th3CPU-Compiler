package com.dafttech.th3cpu.mnemonic

import org.lolhens.parser.ParserUtils

/**
 * Created by LolHens on 25.07.2015.
 */
class MnemonicParser extends ParserUtils {
  var addr: Int = 0
  var labels = Map[String, Byte]()

  def instructions: Parser[List[Byte]] = rep(instruction) ^^ (_.flatten)

  def instruction: Parser[List[Byte]] = const ~ opt(branch) ^^ {
    case const ~ None =>
      addr += 2
      const
    case const ~ Some(branch) =>
      addr += 2
      List((const(0) | branch).toByte, const(1))
  } | move ~ opt(branch) ^^ {
    case move ~ None =>
      addr += 1
      List(move)
    case move ~ Some(branch) =>
      addr += 1
      List((move | branch).toByte)
  } | branch ^^ ((insn) => {
    addr += 1
    List(insn)
  }) | nop ^^ ((byte) => {
    addr += 1
    List(byte)
  }) | byte ^^ ((byte) => {
    addr += 1
    List(byte)
  }) | label ^^ ((label) => {
    labels += label -> addr.toByte
    List()
  })

  private def move: Parser[Byte] = ("mv" | "mov" | "move") ~> optFrame("(", writeRegister ~ opt(",") ~ readRegister, ")") ^^ {
    case target ~ _ ~ source => ((target << 3) | source).toByte
  }

  private val paramRegister = 4

  private def const: Parser[List[Byte]] = "const" ~> optFrame("(", writeRegister ~ opt(",") ~ (byteType | wordType), ")") ^^ {
    case target ~ _ ~ (const: Byte) => List(((target << 3) | paramRegister).toByte, const)
    case target ~ _ ~ (label: String) => List(((target << 3) | paramRegister).toByte, labels(label))
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

  private def nop: Parser[Byte] = "nop" ^^ ((_) => 0)

  private def byte: Parser[Byte] = "byte" ~> optFrame("(", byteType, ")")

  private def label: Parser[String] = "label" ~> optFrame("(", wordType, ")")

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
