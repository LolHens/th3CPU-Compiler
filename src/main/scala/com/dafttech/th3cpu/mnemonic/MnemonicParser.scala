package com.dafttech.th3cpu.mnemonic

import com.dafttech.th3cpu.mnemonic.MnemonicParser.Label
import org.lolhens.parser.ParserUtils

import scala.collection.mutable.ListBuffer

/**
 * Created by LolHens on 25.07.2015.
 */
class MnemonicParser extends ParserUtils {
  var addr: Int = 0
  var labels = Map[String, Label]()

  def instructions: Parser[List[Byte]] = rep(instruction) ^^ ((insnLists) => {
    val insnList = insnLists.flatten

    val buffer = insnList.to[ListBuffer]

    for ((name, label) <- labels;
         occurence <- label.occurences)
      buffer(occurence) = label.target.toByte

    buffer.toList
  })

  def instruction: Parser[List[Byte]] = const ~ opt(branch) ^^ {
    case const ~ optBranch =>
      addr += 2
      optBranch match {
        case None => const
        case Some(branch) => List((const(0) | branch).toByte, const(1))
      }
  } | move ~ opt(branch) ^^ {
    case move ~ optBranch =>
      addr += 1
      optBranch match {
        case None => List(move)
        case Some(branch) => List((move | branch).toByte)
      }
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
    labels.getOrElse(label, {
      val newLabel = new Label
      labels += label -> newLabel
      newLabel
    }).target = addr

    List()
  }) | comment ^^ ((_) => {
    List()
  })

  private def move: Parser[Byte] = ("mv" | "mov" | "move") ~> optFrame("(", writeRegister ~ opt(",") ~ readRegister, ")") <~ opt(comment) ^^ {
    case target ~ _ ~ source => ((target << 3) | source).toByte
  }

  private val paramRegister = 4

  private def const: Parser[List[Byte]] = "const" ~> optFrame("(", writeRegister ~ opt(",") ~ (byteType | paramTextType), ")") <~ opt(comment) ^^ {
    case target ~ _ ~ (const: Byte) => List(((target << 3) | paramRegister).toByte, const)
    case target ~ _ ~ (labelName: String) =>
      val label = labels.getOrElse(labelName, {
        val newLabel = new Label
        labels += labelName -> newLabel
        newLabel
      })
      label.occurences = label.occurences :+ (addr + 1)

      List(((target << 3) | paramRegister).toByte, 0)
  }

  private def branch: Parser[Byte] = (("jmp" | "jump") | "breq" | "brne") <~ opt(comment) ^^ ((jmp) => {
    val byte = jmp match {
      case "jmp" | "jump" => 1
      case "breq" => 2
      case "brne" => 3
      case _ => 0
    }
    (byte << 6).toByte
  })

  private def nop: Parser[Byte] = "nop" <~ opt(comment) ^^ ((_) => 0)

  private def byte: Parser[Byte] = "byte" ~> optFrame("(", byteType, ")") <~ opt(comment)

  private def label: Parser[String] = "label" ~> optFrame("(", paramTextType, ")") <~ opt(comment)

  private def comment: Parser[Unit] = ((";" | "//") ~ textType("", "")) ^^ ((_) => ())

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

  def paramTextType = textType("(", List(")", ",", " ", ";", "//"))
}

object MnemonicParser {

  class Label {
    var target: Int = 0
    var occurences: List[Int] = Nil
  }

}