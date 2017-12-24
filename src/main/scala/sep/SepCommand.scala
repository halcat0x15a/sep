package sep

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.image.BufferedImage

trait SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage]
}

class Compose extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
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
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case Nil => Nil
      case emoji :: stack =>
        val image = createEmoji
        val angle = arguments.getInt("angle").getOrElse(0)
        val op = new AffineTransformOp(AffineTransform.getRotateInstance(math.toRadians(angle), image.getWidth / 2, image.getHeight / 2), AffineTransformOp.TYPE_BILINEAR)
        val g = image.createGraphics
        g.drawImage(op.filter(emoji, null), 0, 0, null)
        g.dispose()
        image :: stack
    }
  }
}

class Chromakey extends SepCommand {
  def filter(source: BufferedImage, key: Color, threshold: Int): BufferedImage = {
    val image = createEmoji
    for (x <- 0 until source.getWidth) {
      for (y <- 0 until source.getHeight) {
        val color = new Color(source.getRGB(x, y), true)
        val delta = math.sqrt(math.pow(color.getRed - key.getRed, 2) + math.pow(color.getGreen - key.getGreen, 2) + math.pow(color.getBlue - key.getBlue, 2))
        if (delta > threshold) {
          image.setRGB(x, y, color.getRGB)
        }
      }
    }
    image
  }

  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case Nil => Nil
      case emoji :: stack =>
        val color = arguments.getColor("color").getOrElse(Color.WHITE)
        val threshold = arguments.getInt("threshold").getOrElse(0)
        filter(emoji, color, threshold) :: stack
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

class Invert extends SepCommand {
  def filter(source: BufferedImage): BufferedImage = {
    val image = createEmoji
    for (x <- 0 until source.getWidth) {
      for (y <- 0 until source.getHeight) {
        val color = new Color(source.getRGB(x, y), true)
        image.setRGB(x, y, new Color(255 - color.getRed, 255 - color.getGreen, 255 - color.getBlue, color.getAlpha).getRGB)
      }
    }
    image
  }

  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case Nil => Nil
      case emoji :: tail => filter(emoji) :: tail
    }
  }
}

class Gray extends SepCommand {
  def apply(stack: List[BufferedImage], arguments: ArgumentStack): List[BufferedImage] = {
    stack match {
      case Nil => Nil
      case emoji :: tail =>
        val image = new BufferedImage(EmojiSize, EmojiSize, BufferedImage.TYPE_BYTE_GRAY)
        val g = image.getGraphics()
        g.drawImage(emoji, 0, 0, null)
        g.dispose()
        image :: tail
    }
  }
}
