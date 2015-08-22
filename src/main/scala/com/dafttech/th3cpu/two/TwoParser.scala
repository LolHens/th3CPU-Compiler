package com.dafttech.th3cpu.two

import org.lolhens.parser.ParserUtils

import scala.collection.mutable.ListBuffer

/**
 * Created by LolHens on 29.07.2015.
 */
class TwoParser extends ParserUtils {
  var ifCount: Int = 0

  def instructions: Parser[List[String]] = rep(instruction) ^^ (_.flatten)

  def instruction: Parser[List[String]] = expr <~ comment | comment ^^ ((_) => List())

  def expr: Parser[List[String]] = optFrame("(", unitExprs | ifExpr | valueExpr | op1Expr | op2Expr | constExpr, ")")

  /*
 val test = 5 + (if (3 > 1) (a + b) else (a + c))

  val nl1 = 3 > 1
  branch1: val nl2 = a + b
  branch2: val nl2 = a + c
  val test = 5 + nl2

  If:
  mov ds, <flags> ;Set the DS to the jump flags
  breq ifTrue
  ;label(ifFalse) ist kein wirkliches label
    ;False code
  jmp ifEnd
  label(ifTrue)
    ;True Code
  label(ifEnd)

   */

  private def ifExpr: Parser[List[String]] = "if" ~> "(" ~> expr ~ ")" ~ expr ~ opt("else" ~ expr) ^^ {
    case bool ~ ")" ~ thenExpr ~ optElse =>
      val buffer = ListBuffer[String]()

      buffer ++= bool
      buffer ++= pop("gpr0")

      buffer += "const(ds, 0b00100000)"
      buffer += "mov(mem_bus, gpr0)"

      buffer += "const(ds, 0b00110001)"
      buffer += "const(mem_bus, 0)"

      buffer += "const(ds, 0b00110001)"
      buffer += "mov(ds, mem_bus)"

      buffer += s"const(ptr, ifFalse$ifCount)"
      buffer += "breq"

      buffer ++= thenExpr

      buffer += s"const(ptr, ifEnd$ifCount)"
      buffer += "jmp"

      buffer += s"label(ifFalse$ifCount)"

      optElse match {
        case Some("else" ~ elseExpr) =>
          buffer ++= elseExpr
        case None =>
        case _ => // TODO: match not exhaustive
      }

      buffer += s"label(ifEnd$ifCount)"

      ifCount += 1

      buffer.toList
  }

  private def constExpr: Parser[List[String]] = byteType ^^ ((byte) => List(s"const(gpr0, $byte)"))

  private def op1Expr: Parser[List[String]] = expr ~ ("*" | "/") ~ expr ^^ {
    case a ~ "*" ~ b => op(a, b, ???)
    case a ~ "/" ~ b => op(a, b, ???)
  }

  private def op2Expr: Parser[List[String]] = expr ~ ("+" | "-") ~ expr ^^ {
    case a ~ "+" ~ b => op(a, b, ???)
    case a ~ "-" ~ b => op(a, b, ???)
  }

  private def op(a: List[String], b: List[String], opReg: Byte): List[String] = {
    val buffer = ListBuffer[String]()
    buffer ++= a
    buffer += "const(ds, 0b00100000)"
    buffer += "mov(mem_bus, gpr0)"
    buffer ++= b // TODO
    buffer += "const(ds, 0b00101000)"
    buffer += "mov(mem_bus, gpr0)"
    buffer += s"const(ds, $opReg)"
    buffer += "mov(gpr0, mem_bus)"
    buffer.toList
  }

  private val addrStackSize = 0xFFFF

  private def pop(register: String) = List(
    // get stack size
    s"const(ds, 0)",
    s"const(ptr, ${addrStackSize - 1})",
    "mov(gpr2, mem_bus)",
    s"const(ds, 0)",
    s"const(ptr, ${addrStackSize})",
    "mov(gpr3, mem_bus)",

    // read from stack
    "mov(ds, gpr2)",
    "mov(ptr,  gpr3)",
    s"mov($register, mem_bus)",

    // decrease stack size (alu)
    "const(ds, 0b00100000)",
    "mov(mem_bus, gpr3)",
    "const(ds, 0b00101000)",
    "const(mem_bus, 1)",
    "const(ds, 0b00111000)",
    "mov(gpr3, mem_bus)",

    // write stack size
    s"const(ds, ${addrStackSize - 1})",
    s"const(ptr, 0)",
    "mov(mem_bus, gpr2)",
    s"const(ds, ${addrStackSize})",
    s"const(ptr, 0)",
    "mov(mem_bus, gpr3)"
  )

  private def push(register: String) = List(
    // get stack size
    s"const(ds, 0)",
    s"const(ptr, ${addrStackSize - 1})",
    "mov(gpr2, mem_bus)",
    s"const(ds, 0)",
    s"const(ptr, ${addrStackSize})",
    "mov(gpr3, mem_bus)",

    // write to stack
    "mov(ds, gpr2)",
    "mov(ptr,  gpr3)",
    s"mov(mem_bus, $register)",

    // increase stack size (alu)
    "",

    // write stack size
    s"const(ds, ${addrStackSize - 1})",
    s"const(ptr, 0)",
    "mov(mem_bus, gpr2)",
    s"const(ds, ${addrStackSize})",
    s"const(ptr, 0)",
    "mov(mem_bus, gpr3)"
  )

  private def valueExpr: Parser[List[String]] = ???

  private def unitExprs: Parser[List[String]] = declUnit | defUnit

  private def declUnit: Parser[List[String]] = varUnit

  private def varUnit: Parser[List[String]] = "var" ~ variable ~ "=" ~ expr ^^ ???

  private def valUnit: Parser[List[String]] = "val" ~ variable ~ "=" ~ expr ^^ ???

  private def defUnit: Parser[List[String]] = variable ~ "=" ~ expr ^^ ???

  private def variable = textType(List("(", " "), List(")", " "))

  private def comment: Parser[Unit] = "//" ~ textType("", "") ^^ ((_) => ())
}
