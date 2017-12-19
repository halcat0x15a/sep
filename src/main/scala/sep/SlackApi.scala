package sep

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.Source
import java.nio.file.Path
import play.api.libs.ws.ahc.AhcWSClient
import play.api.libs.json.JsValue
import play.api.mvc.MultipartFormData
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class SlackApi(accessToken: String) {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  val wsClient = AhcWSClient()

  import system.dispatcher

  def emojiList(): Future[EmojiList] = {
    wsClient.url("https://slack.com/api/emoji.list").withQueryStringParameters("token" -> accessToken).get().map { response =>
      (response.json \ "emoji").toOption match {
        case Some(json) => EmojiList(json)
        case None => throw new SepException("emoji.list のリクエストに失敗したよ")
      }
    }
  }

  def upload(channel: String, name: String, ext: String, path: Path) = {
    val source = Source(List(
      MultipartFormData.FilePart("file", s"${name}.${ext}", Some(s"image/${ext}"), FileIO.fromPath(path)),
      MultipartFormData.DataPart("token", accessToken),
      MultipartFormData.DataPart("channels", channel)
    ))
    wsClient.url("https://slack.com/api/files.upload").post(source)
  }
}
