package sep

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.awt.image.LookupOp

case class Arguments(args: List[ArgumentToken]) {
  def get(name: String): Option[ArgumentToken] = {
    args.find(_.key == name)
  }

  def apply(name: String): ArgumentToken = {
    get(name).getOrElse(throw new SepException(s"${name} が指定されていません"))
  }
}

trait SepCommand {
  def apply(stack: List[BufferedImage], arguments: Arguments): List[BufferedImage]
}

class Compose extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: Arguments): List[BufferedImage] = {
    val image = createEmoji
    val g = image.createGraphics
    stack.foreach { emoji =>
      g.drawImage(emoji, 0, 0, null)
    }
    g.dispose()
    List(image)
  }
}

class Rotate extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: Arguments): List[BufferedImage] = {
    stack match {
      case emoji :: tail =>
        val image = createEmoji
        val g = image.createGraphics
        val angle = arguments.get("angle").flatMap(_.value).fold(0)(_.toInt)
        val op = new AffineTransformOp(AffineTransform.getRotateInstance(math.toRadians(angle), image.getWidth / 2, image.getHeight / 2), AffineTransformOp.TYPE_BILINEAR)
        g.drawImage(op.filter(emoji, null), 0, 0, null)
        g.dispose()
        image :: tail
      case Nil => Nil
    }
  }
}

class Chromakey extends SepCommand {
  def filter(source: BufferedImage, key: Color): BufferedImage = {
    val image = createEmoji
    for (x <- 0 until source.getWidth) {
      for (y <- 0 until source.getHeight) {
        val color = new Color(source.getRGB(x, y), true)
        if (color != key) {
          image.setRGB(x, y, color.getRGB)
        }
      }
    }
    image
  }

  def apply(stack: List[BufferedImage], arguments: Arguments): List[BufferedImage] = {
    stack match {
      case emoji :: tail =>
        val color = new Color(Integer.decode(arguments.get("color").flatMap(_.value).getOrElse("0xffffff")))
        filter(emoji, color) :: tail
      case Nil => Nil
    }
  }
}

class Fill extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: Arguments): List[BufferedImage] = {
    val image = createEmoji
    val g = image.createGraphics
    g.setColor(new Color(Integer.decode(arguments.get("color").flatMap(_.value).getOrElse("0xffffff"))))
    g.fillRect(0, 0, EmojiSize, EmojiSize)
    g.dispose()
    image :: stack
  }
}
