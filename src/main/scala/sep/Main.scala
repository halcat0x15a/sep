package sep

import java.nio.file.Files
import javax.imageio.ImageIO
import javax.imageio.stream.FileImageOutputStream
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

object Main {
  def run(accessToken: String, channel: String, input: String): Future[Unit] = {
    val api = new SlackApi(accessToken)
    import api.system.dispatcher
    for {
      emojiList <- api.emojiList()
      _ <- {
        val tokens = SepParser(input)
        val sep = SlackEmojiProcessor(tokens, emojiList)
        val arguments = Arguments(sep.argumentStack)
        val delayTime = arguments.get("delay").flatMap(_.value).fold(100)(_.toInt)
        if (sep.emojiStack.nonEmpty || sep.frameStack.nonEmpty) {
          val temp = Files.createTempFile("emoji", "")
          val ext = if (sep.frameStack.nonEmpty) {
            val output = new FileImageOutputStream(temp.toFile)
            val writer = new GifSequenceWriter(output, sep.frameStack.head.getType, delayTime, true)
            for (image <- sep.frameStack) {
              writer.writeToSequence(image)
            }
            writer.close()
            output.close()
            "gif"
          } else {
            val emoji = sep.emojiStack.head
            ImageIO.write(emoji, "PNG", temp.toFile)
            "png"
          }
          val name = tokens.collect { case EmojiToken(name) => name }.distinct.mkString("_")
          api.upload(channel, name, ext, temp).andThen { case _ => Files.delete(temp) }
        } else {
          Future.successful(())
        }
      }
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    try {
      Await.result(run(args(0), args(1), args.drop(2).mkString(" ")), Duration.Inf)
      System.exit(0)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        System.exit(1)
    }
  }
}
