package org.lolhens.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by LolHens on 25.07.2015.
 */
class ParserUtils extends JavaTokenParsers {
  def optFrame[T](pre: Parser[Any], main: Parser[T], post: Parser[Any]): Parser[T] = pre ~> main <~ post | main

  /*def flatten(compOpt: Parser[~[Any, Option[Any]]]): Parser[Any] = compOpt ^^ {
    case a ~ None => a
    case a ~ Some(b) => new ~(a, b)
  }*/

  /*def toLowerCase[T](parser: Parser[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = in match {
      case string: String => parser(string.toLowerCase().asInstanceOf[Input])
      case other => parser(other)
    }
  }*/

  def binaryNumber: Parser[String] = """[01]+""".r

  def hexNumber: Parser[String] = """[0-9a-fA-F]+""".r

  def intType: Parser[Int] = (
    "0b" ~> binaryNumber ^^ (Integer.parseInt(_, 2))
      | "0x" ~> hexNumber ^^ ((hexNum) => Integer.parseInt("0" + hexNum, 16))
      | wholeNumber ^^ (_.toInt)
    )

  def shortType: Parser[Short] = intType ^^ (_.toShort)

  def byteType: Parser[Byte] = intType ^^ (_.toByte)

  def doubleType: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def floatType: Parser[Float] = floatingPointNumber ^^ (_.toFloat)

  def booleanType: Parser[Boolean] = ("true" | "false") ^^ {
    case "true" => true
    case "false" => false
  }

  def stringType: Parser[String] = stringLiteral

  //def textType: Parser[String] = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+""".r

  def wordType: Parser[String] = """[0-9a-zA-Z]+""".r
}
