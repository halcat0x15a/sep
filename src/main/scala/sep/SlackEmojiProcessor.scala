package sep

import java.awt.image.BufferedImage
import play.api.libs.json.JsValue
import scala.annotation.tailrec

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

class SepException(message: String) extends Exception(message)

class SlackEmojiProcessor(
  val emojiStack: List[BufferedImage],
  val argumentStack: List[ArgumentToken],
  val frameStack: List[BufferedImage]
) {
  def apply(token: SepToken, emojiList: EmojiList, commandMap: Map[String, SepCommand]): SlackEmojiProcessor = {
    token match {
      case EmojiToken(name) =>
        if (emojiStack.size < 10) {
          val emoji = emojiList.get(name).getOrElse(throw new SepException(s"${name} なんてカスタム絵文字ないよ"))
          new SlackEmojiProcessor(emoji :: emojiStack, argumentStack, frameStack)
        } else {
          throw new SepException("絵文字スタックが溢れちゃった")
        }
      case argument@ArgumentToken(_, _) =>
        new SlackEmojiProcessor(emojiStack, argument :: argumentStack, frameStack)
      case CommandToken(name) =>
        val command = commandMap.get(name).getOrElse(throw new SepException(s"${name} なんてコマンドないよ"))
        new SlackEmojiProcessor(command(emojiStack, Arguments(argumentStack)), Nil, frameStack)
      case FrameToken =>
        emojiStack match {
          case Nil => this
          case emoji :: stack =>
            new SlackEmojiProcessor(stack, argumentStack, emoji :: frameStack)
        }
    }
  }
}

object SlackEmojiProcessor {
  val commandMap: Map[String, SepCommand] = Map(
    "compose" -> new Compose,
    "rotate" -> new Rotate,
    "chromakey" -> new Chromakey,
    "fill" -> new Fill
  )

  def apply(tokens: Seq[SepToken], emojiList: EmojiList): SlackEmojiProcessor = {
    tokens.foldLeft(new SlackEmojiProcessor(Nil, Nil, Nil)) { (sep, token) =>
      sep(token, emojiList, commandMap)
    }
  }
}
