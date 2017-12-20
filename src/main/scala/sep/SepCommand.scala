package sep

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage
import java.awt.image.LookupOp

trait SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage]
}

class Compose extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    val image = createEmoji
    val g = image.createGraphics
    if (arguments.get("all").isDefined) {
      stack.foreach { emoji =>
        g.drawImage(emoji, 0, 0, null)
      }
      g.dispose()
      List(image)
    } else {
      stack match {
        case x :: y :: tail =>
          g.drawImage(x, 0, 0, null)
          g.drawImage(y, 0, 0, null)
          g.dispose()
          image :: tail
        case _ =>
          g.dispose()
          stack
      }
    }
  }
}

class Rotate extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case emoji :: tail =>
        val image = createEmoji
        val g = image.createGraphics
        val angle = arguments.getInt("angle").getOrElse(0)
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

  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case emoji :: tail =>
        val color = arguments.getColor("color").getOrElse(Color.WHITE)
        filter(emoji, color) :: tail
      case Nil => Nil
    }
  }
}

class Fill extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    val image = createEmoji
    val g = image.createGraphics
    g.setColor(arguments.getColor("color").getOrElse(Color.WHITE))
    g.fillRect(0, 0, EmojiSize, EmojiSize)
    g.dispose()
    image :: stack
  }
}

class Duplicate extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case Nil => Nil
      case emoji :: tail =>
        val head = new BufferedImage(emoji.getColorModel, emoji.copyData(null), emoji.getColorModel.isAlphaPremultiplied, null)
        head :: emoji :: tail
    }
  }
}
