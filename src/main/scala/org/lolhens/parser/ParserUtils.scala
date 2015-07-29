package org.lolhens.parser

import java.util.regex.Pattern

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by LolHens on 25.07.2015.
 */
class ParserUtils extends JavaTokenParsers {
  def optFrame[T](pre: Parser[Any], main: Parser[T], post: Parser[Any]): Parser[T] = (pre ~> main <~ post) | main

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

  def binaryNumber: Parser[String] = "[01]+".r

  def hexNumber: Parser[String] = "[0-9a-fA-F]+".r

  def intType: Parser[Int] = (
    "0b" ~> binaryNumber ^^ (Integer.parseInt(_, 2))
      | "0x" ~> hexNumber ^^ ((hexNum) => Integer.parseInt("0" + hexNum, 16))
      | wholeNumber ^^ (_.toInt)
    )

  def shortType: Parser[Short] = intType ^^ (_.toShort)

  def byteType: Parser[Byte] = intType ^^ (_.toByte)

  def doubleType: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[dD]?""".r ^^ (_.toDouble)

  def floatType: Parser[Float] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fF]?""".r ^^ (_.toFloat)

  def booleanType: Parser[Boolean] = ("true" | "false") ^^ {
    case "true" => true
    case "false" => false
  }

  def stringType: Parser[String] = stringLiteral

  def wordType: Parser[String] = """(\w)+""".r

  def textType(pre: List[String], post: List[String]): Parser[String] = s"${except(pre).getOrElse("")}${except(post).getOrElse(".")}+".r

  protected def except(list: List[String]): Option[String] = {
    val except = list.map(quote).mkString("")
    if (except == "") None else Some(s"[^$except]")
  }

  protected def quote(string: String) = if (string == "") "" else Pattern.quote(string)

  implicit def stringToStringList(string: String): List[String] = List(string)
}
