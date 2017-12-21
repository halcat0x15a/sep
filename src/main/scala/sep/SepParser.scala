package sep

import scala.util.parsing.combinator.RegexParsers

sealed abstract class SepToken
case class EmojiToken(name: String) extends SepToken
case class CommandToken(name: String) extends SepToken
case class ArgumentToken(key: String, value: String) extends SepToken
case object FrameToken extends SepToken

object SepParser extends RegexParsers {
  val word: Parser[String] = """[\w#-]+""".r

  def emoji: Parser[SepToken] = ':' ~> word <~ ':' ^^ { name => EmojiToken(name) }

  def option: Parser[SepToken] = "--" ~> word ^^ { key => ArgumentToken(key, "") }

  def keyValue: Parser[SepToken] = "--" ~> (word <~ '=') ~ word ^^ {
    case key ~ value => ArgumentToken(key, value)
  }

  def argument: Parser[SepToken] = keyValue | option

  def command: Parser[SepToken] = word ^^ { name => CommandToken(name) }

  def frame: Parser[SepToken] = ";" ^^^ FrameToken

  def token: Parser[SepToken] = frame | emoji | argument | command

  override def skipWhitespace = false

  def apply(input: String): List[SepToken] = parseAll(repsep(token, whiteSpace), input) match {
    case Success(result, _) => result
    case failure: NoSuccess => throw new SepException(failure.msg)
  }
}
