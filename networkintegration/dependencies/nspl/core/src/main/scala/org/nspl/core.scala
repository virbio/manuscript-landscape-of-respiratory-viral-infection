package org.nspl

case class Bounds(
    x: Double,
    y: Double,
    w: Double,
    h: Double,
    anchor: Option[Point] = None
) {
  def maxX = x + w
  def maxY = y + h
  def centerX = x + w * 0.5
  def centerY = y + h * 0.5
  def contains(p: Point) =
    p.x >= x && p.x <= x + w &&
      p.y >= y && p.y <= y + h

  

}

object Bounds {
  def fromPoints(p1: Point, p2:Point) = {
    val x = math.min(p1.x,p2.x)
    val y = math.min(p1.y,p2.y)
    val w = math.abs(p1.x - p2.x)
    val h = math.abs(p1.y - p2.y)
    Bounds(x,y,w,h,None)
  }
}

sealed trait Cap
case object CapButt extends Cap
case object CapSquare extends Cap
case object CapRound extends Cap

case class Stroke(
    width: RelFontSize,
    cap: Cap = CapButt,
    dashArray: List[Double] = Nil
)

case class Point(x: Double, y: Double) {
  def directionTo(other: Point) = {
    val dx = other.x - x
    val dy = other.y - y
    val norm = math.sqrt(dx * dx + dy * dy)
    (dx / norm, dy / norm)
  }
  def translate(dx: Double, dy: Double) = Point(x + dx, y + dy)
  def transform(tx: AffineTransform) = tx.transform(this)
  def transform(tx: Bounds => AffineTransform) =
    tx(Bounds(x, y, 0, 0)).transform(this)
  def distance(p: Point) = {
    val a = (x - p.x)
    val b = (y - p.y)
    math.sqrt(a * a + b * b)
  }
}

trait RenderingContext[A <: RenderingContext[A]] { self: A =>
  type LocalTx
  def concatTransform(tx: AffineTransform): Unit
  def setTransform(tx: LocalTx): Unit
  def getTransform: LocalTx
  def localToScala(tx: LocalTx): AffineTransform
  def getAffineTransform: AffineTransform = localToScala(getTransform)
  def withTransform[T](tx: AffineTransform)(f: => T) = {
    val current = getTransform
    concatTransform(tx)
    val r = f
    setTransform(current)
    r
  }
  def render[K <: Renderable[K]](k: K)(implicit r: Renderer[K, A]) =
    r.render(self, k)
}

trait Descriptor
case object NoDescriptor extends Descriptor
case class DataSourceIdx(externalDataSourceIdx: Int, i: Int) 
case class DataRowIdx(externalDataSourceIdx: Int, dataSourceIdx: Int, rowIdx: Int) extends Descriptor
case class PlotAreaDescriptor(
    id: AnyRef,
    xAxis: Axis,
    yAxis: Axis,
    bounds: Option[Bounds],
    canFuseEvents: Boolean
) extends Descriptor

trait Renderer[E, R <: RenderingContext[R]] {
  def render(r: R, e: E): Unit
}

/* Basic unit of the scene graph.*/
trait Renderable[K] { self: K =>
  def transform(v: Bounds => AffineTransform): K
  def bounds: Bounds

  def translate(x: Double, y: Double) =
    transform(_ => AffineTransform.translate(x, y))
  def scale(x: Double, y: Double) = transform(_ => AffineTransform.scale(x, y))
  def rotate(rad: Double, x: Double, y: Double) =
    transform(_ => AffineTransform.rotate(rad, x, y))
  def rotate(rad: Double) = transform(_ => AffineTransform.rotate(rad))
  def reflectOrigin = transform(_ => AffineTransform.reflectOrigin)
  def reflectY = transform(_ => AffineTransform.reflectY)
  def reflectX = transform(_ => AffineTransform.reflectX)
  def reflectXCenter = transform(b => AffineTransform.reflectXCenter(b))
  def reflectYCenter = transform(b => AffineTransform.reflectYCenter(b))
  def rotateCenter(rad: Double) =
    transform(b => AffineTransform.rotateCenter(rad)(b))

}

/* Layouts tranform the bounding box of their members. */
trait Layout {
  def apply[F: FC](s: Seq[Bounds]): Seq[Bounds]
}

case class RelFontSize(private val v: Double) extends AnyVal {
  def *(t: Double) = RelFontSize(v * t)
  def value(implicit fc: FontConfiguration) = v * fc.font.size
  def factor = v
}
case class BaseFontSize(v: Int) extends AnyVal
