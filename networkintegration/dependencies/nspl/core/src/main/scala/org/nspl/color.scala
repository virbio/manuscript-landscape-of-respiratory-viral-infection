package org.nspl

trait Colormap { self =>
  def apply(v: Double): Color
  def withRange(min: Double, max: Double): Colormap
  def reverse(max: Double, min: Double): Colormap = new Colormap {
    def withRange(min: Double, max: Double): Colormap =
      self.withRange(min, max).reverse(min, max)
    def apply(v: Double) = {
      val v2 = min + (1d - (v - min) / (max - min)) * (max - min)
      self.apply(v2)
    }
  }
  def mapColors(f: Color => Color) = Colormap.map(self)(f)
}
object Colormap {
  def map(cm: Colormap)(f: Color => Color) = new Colormap {
    def apply(v: Double) = f(cm.apply(v))
    def withRange(min: Double, max: Double) = this
  }
}
case class Color(r: Int, g: Int, b: Int, a: Int) extends Colormap {
  def apply(v: Double) = this
  def withRange(min: Double, max: Double) = this
}
object Color {
  def apply(r: Int, g: Int, b: Int): Color = Color(r, g, b, 255)
  val black = Color(0, 0, 0, 255)
  val white = Color(255, 255, 255, 255)
  val transparent = Color(0, 0, 0, 0)
  val red = Color(255, 0, 0, 255)
  val blue = Color(0, 0, 255, 255)
  val green = Color(0, 255, 0, 255)
  val gray1 = Color(50, 50, 50, 255)
  val gray2 = Color(100, 100, 100, 255)
  val gray3 = Color(150, 150, 150, 255)
  val gray4 = Color(200, 200, 200, 255)
  val gray5 = Color(220, 220, 220, 255)
  val gray6 = Color(230, 230, 230, 255)
  val grey1 = Color(50, 50, 50, 255)
  val grey2 = Color(100, 100, 100, 255)
  val grey3 = Color(150, 150, 150, 255)
  val grey4 = Color(200, 200, 200, 255)
  val grey5 = Color(220, 220, 220, 255)
  val grey6 = Color(230, 230, 230, 255)
  val BLACK = black
  val WHITE = white
  val RED = red
  val GREEN = green
  val BLUE = blue
  def interpolate(start: Int, end: Int, t: Double) =
    ((end - start) * t + start).toInt
  def interpolateInRGB(from: Color, to: Color, t: Double) = {
    Color(
      interpolate(from.r, to.r, t),
      interpolate(from.g, to.g, t),
      interpolate(from.b, to.b, t)
    )
  }

}

case class ManualColor(map: Map[Double, Color], default: Color = Color.gray5)
    extends Colormap {
  def apply(v: Double) =
    if (v.isNaN) Color.transparent else map.getOrElse(v, default)
  def withRange(min: Double, max: Double) = this
}
object ManualColor {
  def apply(colors: Color*): ManualColor =
    ManualColor(colors.zipWithIndex.map(x => x._2.toDouble -> x._1).toMap)
  def fromFunction(pf: Double => Color) : Colormap = new Colormap {
    def apply(v: Double) = if (v.isNaN) Color.transparent else pf(v)
    def withRange(min: Double, max: Double) = this
  }
}

case class HeatMapColors(
    min: Double = 0.0,
    max: Double = 1.0,
    mid: Option[Double] = None
) extends Colormap {

  def apply(value: Double): Color = {
    if (value.isNaN) Color.transparent
    else {
      val mid1 = mid.getOrElse((min + max) / 2)

      val v =
        if (value > max) 1.0
        else if (value < min) 0.0
        else if (value > mid1) 0.5 * (value - mid1) / (max - mid1) + 0.5
        else (value - min) * 0.5 / (mid1 - min)

      def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

      val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)

      Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
    }
  }

  def withRange(min: Double, max: Double) = HeatMapColors(min, max, mid)
}

case class GrayScale(
    min: Double = 0.0,
    max: Double = 1.0,
    white: Int = 255,
    transparentBelowBounds: Boolean = false
) extends Colormap {

  def apply(value: Double): Color = {
    if (value.isNaN) Color.transparent
    else {

      val v =
        if (value > max) 1.0
        else if (value < min) 0.0
        else (value - min) / (max - min)

      val alpha =
        if (transparentBelowBounds && value < min) 0 else 255

      Color(
        white - (v * white).toInt,
        white - (v * white).toInt,
        white - (v * white).toInt,
        alpha
      )

    }
  }

  def withRange(min: Double, max: Double) = GrayScale(min, max, white)
}

case class RedBlue(
    min: Double = 0.0,
    max: Double = 1.0,
    mid: Option[Double] = None,
    white: Int = 255
) extends Colormap {
  val whiteLevel = white / 255d

  def apply(value: Double): Color = {
    if (value.isNaN) Color.transparent
    else {

      val mid1 = mid.getOrElse((max + min) / 2)

      val midColor = Color(white, white, white)

      if (value > mid1) {
        val v =
          if (value > max) 1.0
          else if (value < min) 0.0
          else (value - mid1) / (max - mid1)
        Color.interpolateInRGB(midColor, Color.red, v)
      } else {
        val v =
          if (value < min) 1.0
          else if (value > max) 0.0
          else (mid1 - value) / (mid1 - min)
        Color.interpolateInRGB(midColor, Color.blue, v)
      }
    }
  }

  def withRange(min: Double, max: Double) = RedBlue(min, max, mid, white)
}
case class Gradient3(
    colorMin: Color,
    colorMid: Color,
    colorMax: Color,
    min: Double,
    max: Double,
    mid: Option[Double]
) extends Colormap {
  def apply(value: Double): Color = {
    if (value.isNaN) Color.transparent
    else {

      val mid1 = mid.getOrElse((max + min) / 2)

      if (value > mid1) {
        val v =
          if (value > max) 1.0
          else if (value < min) 0.0
          else (value - mid1) / (max - mid1)
        Color.interpolateInRGB(colorMid, colorMax, v)
      } else {
        val v =
          if (value < min) 1.0
          else if (value > max) 0.0
          else (mid1 - value) / (mid1 - min)
        Color.interpolateInRGB(colorMid, colorMin, v)
      }
    }
  }

  def withRange(min: Double, max: Double) =
    Gradient3(colorMin, colorMid, colorMax, min, max, mid)
}

object Gradient3 {
  def greyBrown(
      min: Double = 0.0,
      max: Double = 1.0,
      mid: Option[Double] = None
  ) =
    Gradient3(
      Color(209, 219, 233),
      Color(236, 206, 189),
      Color(167, 37, 46),
      min,
      max,
      mid
    )
}

case class LogHeatMapColors(min: Double = 0.0, max: Double = 1.0)
    extends Colormap {

  val min1 = 1d
  val max1 = (max - min) + 1

  def apply(value: Double): Color = {
    if (value.isNaN) Color.transparent
    else {

      val v =
        if (value > max) 1.0
        else if (value < min) 0d
        else math.log10(value - min + 1) / math.log10(max1)

      def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

      val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)

      Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
    }
  }

  def withRange(min: Double, max: Double) = LogHeatMapColors(min, max)
}

case class DiscreteColors(
    numColors: Int,
    saturation: Double = 1.0,
    lighting: Double = 0.5
) extends Colormap {

  def apply(value: Double): Color = {
    val v = if (value >= numColors) math.max(0,numColors - 1) else if (value < 0) 0 else value
    colorPick(v.toInt, numColors, saturation, lighting)
  }

  def withRange(min: Double, max: Double) = DiscreteColors(max.toInt)
}

trait Colors {

  def fromRGBHexString(s: String) = {
    val rs = s.take(2)
    val gs = s.drop(2).take(2)
    val bs = s.drop(4).take(2)
    val r = java.lang.Integer.parseInt(rs, 16)
    val g = java.lang.Integer.parseInt(gs, 16)
    val b = java.lang.Integer.parseInt(bs, 16)
    Color(r, g, b)
  }

  def colorPick(
      idx: Int,
      numColors: Int,
      saturation: Double = 1.0,
      lighting: Double = 0.5
  ) = {
    if (idx >= numColors) Color.gray3
    else if (numColors <= colorList.size ) colorList(idx)
    else
      hslCircle(idx, math.max(0,numColors-1), saturation, lighting)
  }

  def hslCircle(idx: Int, max: Int, saturation: Double, lighting: Double) = {
    val (r, g, b) = hsl2rgb2(idx.toDouble / max.toDouble, saturation, lighting)
    val color = Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
    color
  }

  // https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSL
  def hsl2rgb2(h: Double, s: Double, l: Double) = {
    val h1 = h * 360
    val c = (1 - math.abs(2 * l - 1)) * s
    val hprime = h1 / 60
    val x = c * (1d - math.abs(hprime % 2 - 1))
    val (r1, g1, b1) =
      if (hprime < 1) (c, x, 0d)
      else if (hprime < 2) (x, c, 0d)
      else if (hprime < 3) (0d, c, x)
      else if (hprime < 4) (0d, x, c)
      else if (hprime < 5) (x, 0d, c)
      else if (hprime <= 6) (c, 0d, x)
      else (0d, 0d, 0d)

    val m = l - 0.5 * c
    (r1 + m, g1 + m, b1 + m)
  }

  lazy val colorList = Vector(
     Color(128, 128, 128, 255),
    Color(230, 150, 0, 255),
    Color(86, 180, 233, 255),
    Color(0, 158, 115, 255),
    Color(240, 228, 66, 255),
    Color(0, 114, 178, 255),
    Color(213, 94, 0, 255),
    Color(204, 121, 167, 255),
    Color(0, 128, 128),
    Color(240, 210, 255),
    Color(170, 110, 40),
    Color(255, 250, 200),
    Color(128, 0, 0),
    Color(170, 255, 195),
    Color(0, 0, 128),
  )

}
