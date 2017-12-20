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
  tokens: List[SepToken],
  emojiStack: List[BufferedImage],
  argumentStack: ArgumentStack,
  frameStack: List[BufferedImage],
  step: Int
) {
  @tailrec
  final def run(emojiList: EmojiList, commandMap: Map[String, SepCommand]): SlackEmojiProcessor = {
    if (step > 200) throw new SepException("ステップ数が200を超えました")
    tokens match {
      case Nil => this
      case EmojiToken(name) :: tokens =>
        if (emojiStack.size < 10) {
          val emoji = emojiList.get(name).getOrElse(throw new SepException(s"${name} なんてカスタム絵文字ないよ"))
          copy(tokens = tokens, emojiStack = emoji :: emojiStack, step = step + 1).run(emojiList, commandMap)
        } else {
          throw new SepException("絵文字スタックが溢れました")
        }
      case (argument@ArgumentToken(_, _)) :: tokens =>
        copy(tokens = tokens, argumentStack = argument :: argumentStack).run(emojiList, commandMap)
      case CommandToken("loop") :: tokens =>
        val n = argumentStack.getInt("n").getOrElse(1)
        copy(tokens = List.fill(n)(tokens).flatten).run(emojiList, commandMap)
      case CommandToken(name) :: tokens =>
        val command = commandMap.get(name).getOrElse(throw new SepException(s"${name} なんてコマンドないよ"))
        copy(tokens = tokens, emojiStack = command(emojiStack, argumentStack), argumentStack = ArgumentStack(Map.empty), step = step + 1).run(emojiList, commandMap)
      case FrameToken :: tokens =>
        emojiStack match {
          case Nil => copy(tokens = tokens).run(emojiList, commandMap)
          case emoji :: stack =>
            copy(tokens = tokens, emojiStack = stack, frameStack = emoji :: frameStack).run(emojiList, commandMap)
        }
    }
  }
}

object SlackEmojiProcessor {
  val commandMap: Map[String, SepCommand] = Map(
    "compose" -> new Compose,
    "rotate" -> new Rotate,
    "chromakey" -> new Chromakey,
    "fill" -> new Fill,
    "duplicate" -> new Duplicate
  )

  def apply(tokens: List[SepToken], emojiList: EmojiList): SlackEmojiProcessor = {
    val sep = SlackEmojiProcessor(tokens, Nil, ArgumentStack(Map.empty), Nil, 0)
    sep.run(emojiList, commandMap)
  }
}
