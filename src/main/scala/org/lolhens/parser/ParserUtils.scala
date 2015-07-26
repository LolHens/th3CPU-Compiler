package org.lolhens.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by LolHens on 25.07.2015.
 */
class ParserUtils extends JavaTokenParsers {
  def optFrame[T](pre: Parser[Any], main: Parser[T], post: Parser[Any]): Parser[T] = pre ~> main <~ post | main

  // TODO
  def flatten(compOpt: Parser[~[Any, Option[Any]]]): Parser[Any] = compOpt ^^ {
    case a ~ None => a
    case a ~ Some(b) => new ~(a, b)
  }

  def longType: Parser[Long] = wholeNumber ^^ (_.toLong)

  def intType: Parser[Int] = wholeNumber ^^ (_.toInt)

  def shortType: Parser[Short] = wholeNumber ^^ (_.toShort)

  def byteType: Parser[Byte] = wholeNumber ^^ (_.toByte)

  def doubleType: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def floatType: Parser[Float] = floatingPointNumber ^^ (_.toFloat)

  def booleanType: Parser[Boolean] = ("true" | "false") ^^ {
    case "true" => true
    case "false" => false
  }

  def stringType: Parser[String] = stringLiteral

  def textType: Parser[String] = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+""".r

  def wordType: Parser[String] = """[a-z]+""".r
}
