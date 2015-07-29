package com.dafttech.th3cpu.two

import org.lolhens.parser.ParserUtils

/**
 * Created by LolHens on 29.07.2015.
 */
class TwoParser extends ParserUtils {
  def instructions: Parser[List[String]] = rep(instruction) ^^ (_.flatten)

  def instruction: Parser[List[String]] = expr <~ comment | comment ^^ ((_) => List())

  def expr: Parser[List[String]] = ifExpr | value | op1Expr | op2Expr | constExpr

  private def ifExpr: Parser[List[String]] = "if" ~> "(" ~> expr ~ ")" ~ expr ~ opt("else" ~ expr) ^^ ((_) => ???)

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
    a :::
      "const(ds, 0b00100000)" ::
      "mov(mem_bus, gpr0)" ::
      b :::
      "const(ds, 0b00101000)" ::
      "mov(mem_bus, gpr0)" ::
      "const(ds, ???)" ::
      "mov(gpr0, mem_bus)"
  }

  private def value: Parser[List[String]] = ???

  private def comment: Parser[Unit] = "//" ~ textType("", "") ^^ ((_) => ())
}
