import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.IIOImage
import javax.imageio.ImageIO
import javax.imageio.ImageTypeSpecifier
import javax.imageio.ImageWriter
import javax.imageio.metadata.IIOMetadataNode
import javax.imageio.stream.ImageOutputStream

package object sep {
  val EmojiSize: Int = 128

  def gifWriter: ImageWriter = {
    ImageIO.getImageWritersByFormatName("GIF").next()
  }

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

  def writeGif(output: ImageOutputStream, images: Seq[BufferedImage], delayTime: Int, loopCount: Int) = {
    val specifier = ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_ARGB)
    val writer = gifWriter
    val param = writer.getDefaultWriteParam
    val meta = writer.getDefaultImageMetadata(specifier, param)
    val root = meta.getAsTree(meta.getNativeMetadataFormatName)
    val children = root.getChildNodes
    for (i <- 0 until children.getLength) {
      val node = children.item(i).asInstanceOf[IIOMetadataNode]
      if (node.getNodeName == "GraphicControlExtension") {
        node.setAttribute("delayTime", (delayTime / 10).toString)
      }
    }
    val extension = new IIOMetadataNode("ApplicationExtension")
    extension.setAttribute("applicationID", "NETSCAPE")
    extension.setAttribute("authenticationCode", "2.0")
    extension.setUserObject(Array[Byte](1, (loopCount & 0xFF).toByte, ((loopCount >> 8) & 0xFF).toByte))
    val extensions = new IIOMetadataNode("ApplicationExtensions")
    extensions.appendChild(extension)
    root.appendChild(extensions)
    meta.setFromTree(meta.getNativeMetadataFormatName, root)
    writer.setOutput(output)
    writer.prepareWriteSequence(null)
    for (image <- images) {
      writer.writeToSequence(new IIOImage(image, null, meta), param)
    }
    writer.endWriteSequence()
  }
}
