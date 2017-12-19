package sep

import java.net.URL
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.imageio.ImageReader
import javax.imageio.metadata.IIOMetadata
import javax.imageio.metadata.IIOMetadataNode

sealed abstract class Emoji
case class StillEmoji(image: BufferedImage) extends Emoji
case class AnimatedEmoji(image: Vector[BufferedImage], delayTime: Option[Int], loopCount: Option[Int]) extends Emoji

object Emoji {
  def getElement(meta: IIOMetadata, name: String): Option[IIOMetadataNode] = {
    val tree = meta.getAsTree(meta.getNativeMetadataFormatName)
    val nodes = tree.getChildNodes
    (0 until nodes.getLength).map { i =>
      nodes.item(i).asInstanceOf[IIOMetadataNode]
    }.find(_.getNodeName == name)
  }

  def getDelayTime(meta: IIOMetadata): Option[Int] = {
    getElement(meta, "GraphicControlExtension").map(_.getAttribute("delayTime").toInt)
  }

  def getLoopCount(meta: IIOMetadata): Option[Int] = {
    getElement(meta, "ApplicationExtensions").map { node =>
      val extension = node.getFirstChild
      val userObject = extension.asInstanceOf[IIOMetadataNode].getUserObject.asInstanceOf[Array[Byte]]
      userObject(1) | (userObject(2) << 8)
    }
  }

  def readAll(reader: ImageReader, index: Int, acc: Vector[BufferedImage]): Vector[BufferedImage] = {
    try {
      readAll(reader, index + 1, acc :+ reader.read(index))
    } catch {
      case _: IndexOutOfBoundsException => acc
    }
  }

  def read(url: String): Emoji = {
    val input = ImageIO.createImageInputStream(new URL(url).openStream)
    try {
      val reader = ImageIO.getImageReaders(input).next()
      reader.setInput(input)
      val images = readAll(reader, 0, Vector.empty)
      if (images.size == 1) {
        StillEmoji(images.head)
      } else if (images.size > 0) {
        val meta = reader.getImageMetadata(0)
        AnimatedEmoji(images, getDelayTime(meta), getLoopCount(meta))
      } else {
        throw new SepException("絵文字が空です")
      }
    } finally {
      input.close()
    }
  }
}
