package sep

import java.awt.Color
import java.awt.image.BufferedImage
import play.api.libs.json.JsValue
import scala.annotation.tailrec
import scala.util.Try

case class EmojiList(json: JsValue) {
  val Alias = "alias:(.+)".r

  @tailrec
  final def get(key: String): Option[BufferedImage] = {
    (json \ key).asOpt[String] match {
      case Some(Alias(name)) => get(name)
      case Some(url) => Some(readEmoji(url))
      case None => None
    }
  }
}

case class ArgumentStack(args: Map[String, String]) {
  def ::(argument: ArgumentToken): ArgumentStack = ArgumentStack(args + (argument.key -> argument.value))

  def get(name: String): Option[String] = {
    args.get(name)
  }

  def getInt(name: String): Option[Int] = {
    get(name).flatMap(value => Try(value.toInt).toOption)
  }

  def getColor(name: String): Option[Color] = {
    get(name).flatMap(value => Try(new Color(Integer.decode(value))).toOption)
  }
}

class SepException(message: String) extends Exception(message)

case class SlackEmojiProcessor(
  emojiStack: List[BufferedImage],
  argumentStack: ArgumentStack,
  frameStack: List[BufferedImage]
)

object SlackEmojiProcessor {
  val commandMap: Map[String, SepCommand] = Map(
    "compose" -> new Compose,
    "rotate" -> new Rotate,
    "chromakey" -> new Chromakey,
    "fill" -> new Fill,
    "invert" -> new Invert,
    "gray" -> new Gray
  )

  def apply(tokens: List[SepToken], emojiList: EmojiList): SlackEmojiProcessor = {
    @tailrec
    def loop(tokens: List[SepToken], sep: SlackEmojiProcessor, step: Int): SlackEmojiProcessor = {
      if (step > 200) throw new SepException("ステップ数が200を超えました")
      tokens match {
        case Nil => sep
        case EmojiToken(name) :: tokens =>
          if (sep.emojiStack.size < 10) {
            val emoji = emojiList.get(name).getOrElse(throw new SepException(s"${name} なんてカスタム絵文字ないよ"))
            loop(tokens, sep.copy(emojiStack = emoji :: sep.emojiStack), step + 1)
          } else {
            throw new SepException("絵文字スタックが溢れました")
          }
        case (argument@ArgumentToken(_, _)) :: tokens =>
          loop(tokens, sep.copy(argumentStack = argument :: sep.argumentStack), step)
        case CommandToken(name) :: tokens =>
          val command = commandMap.get(name).getOrElse(throw new SepException(s"${name} なんてコマンドないよ"))
          val emojiStack = command(sep.emojiStack, sep.argumentStack)
          loop(tokens, sep.copy(emojiStack = emojiStack, argumentStack = ArgumentStack(Map.empty)), step + 1)
        case FrameToken :: tokens =>
          sep.emojiStack match {
            case Nil => loop(tokens, sep, step)
            case emoji :: emojiStack =>
              loop(tokens, sep.copy(emojiStack = emojiStack, frameStack = emoji :: sep.frameStack), step)
          }
      }
    }
    loop(tokens, SlackEmojiProcessor(Nil, ArgumentStack(Map.empty), Nil), 0)
  }
}
