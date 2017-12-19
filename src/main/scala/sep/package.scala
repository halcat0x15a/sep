import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO

package object sep {
  val EmojiSize: Int = 128

  def createEmoji: BufferedImage = new BufferedImage(EmojiSize, EmojiSize, BufferedImage.TYPE_INT_ARGB)

  def readEmoji(url: String): BufferedImage = {
    val src = ImageIO.read(new URL(url))
    val dest = createEmoji
    val g = dest.createGraphics
    if (src.getWidth < src.getHeight) {
      val r = src.getWidth.toDouble / src.getHeight
      val w = dest.getWidth * r
      g.drawImage(src, ((dest.getWidth - w) / 2).toInt, 0, w.toInt, dest.getHeight, null)
    } else {
      val r = src.getHeight.toDouble / src.getWidth
      val h = dest.getHeight * r
      g.drawImage(src, 0, ((dest.getHeight - h) / 2).toInt, dest.getWidth, h.toInt, null)
    }
    g.dispose()
    dest
  }
}
