package org

package object nspl
    extends Tuples1
    with Tuples2
    with Colors
    with Shapes
    with Renderers
    with data.DataAdaptors
    with Plots
    with SimplePlots
    with ImplicitConversions
    with Events
    with Renderers3D
    with Plots3D {

  type Build[A] = ((Option[A], Event,Boolean)) => A // Option[A]

  implicit class defaultBuild[T](b: Build[T]) {
    def build: T = b((None , BuildEvent,false))
  }

  type AxisElem =
    Elems3[ShapeElem, ElemList[Elems2[ShapeElem, ElemOption[TextBox]]], ElemList[
      ShapeElem
    ]]

  type FC[_] = FontConfiguration

  implicit def baseFont(implicit fc: FontConfiguration): BaseFontSize =
    BaseFontSize(fc.font.size)

  implicit class ConvD(v: Double) {
    def fts = RelFontSize(v)
  }
  implicit class ConvI(v: Int) {
    def fts = RelFontSize(v.toDouble)
  }
  implicit class ConvRFS(v: RelFontSize) {
    def value(implicit bs: FontConfiguration) = v.value
  }

  def font(name: String)(implicit gm: GlyphMeasurer[NamedFont#F]) =
    GenericFontConfig(NamedFont(name, 10))(gm)

  def mapEvent[A <: Renderable[A], B <: Renderable[B]](
      old: (Option[A], Event)
  )(f: A => B): (Option[B], Event) = old match {
    case (None, BuildEvent) => None -> BuildEvent
    case (Some(old), e) =>
      val b = f(old)
      Some(b) -> e
    case _ => throw new RuntimeException("should not happen")
  }

  /* Calculates the total bounds of the members. */
  def outline(members1: Iterator[Bounds], anchor: Option[Point]) = {
    var empty = true
    var minX = Double.MaxValue
    var minY = Double.MaxValue
    var maxX = Double.MinValue
    var maxY = Double.MinValue

    members1.foreach { t =>
      if (t.w > 0 || t.h > 0) {
        empty = false
        if (t.x < minX) {
          minX = t.x
        }
        if (t.maxX > maxX) {
          maxX = t.maxX
        }
        if (t.y < minY) {
          minY = t.y
        }
        if (t.maxY > maxY) {
          maxY = t.maxY
        }
      }
    }

    if (empty) Bounds(0, 0, 0, 0, anchor)
    else {

      val w = maxX - minX
      val h = maxY - minY
      Bounds(minX, minY, w, h, anchor)
    }
  }

  def transform[T <: Renderable[T]](
      member: T,
      tx: Bounds => AffineTransform
  ): T =
    member.transform(tx)

  def translate[T <: Renderable[T]](member: T, x: Double, y: Double): T =
    member.translate(x, y)

  def rotate[T <: Renderable[T]](
      member: T,
      rad: Double,
      x: Double,
      y: Double
  ): T = member.rotate(rad, x, y)

  def rotate[T <: Renderable[T]](member: T, rad: Double) =
    member.rotate(rad)

  def reflectOrigin[T <: Renderable[T]](member: T) =
    member.reflectOrigin

  def reflectX[T <: Renderable[T]](member: T) =
    member.reflectX

  def rotateCenter[T <: Renderable[T]](member: T, rad: Double) =
    member.rotateCenter(rad)

  def reflectY[T <: Renderable[T]](member: T) =
    member.reflectY

  def scale[T <: Renderable[T]](member: T, x: Double, y: Double) =
    member.scale(x, y)

  def fitToBounds[T <: Renderable[T]](member: T, bounds: Bounds) = {
    val current = member.bounds
    val scaled = scale(
      member,
      if (current.w != 0d) bounds.w / current.w else 1d,
      if (current.h != 0d) bounds.h / current.h else 1d
    )
    val scaledB = scaled.bounds
    translate(scaled, bounds.x - scaledB.x, bounds.y - scaledB.y)
  }

  implicit def renderable2build[T <: Renderable[T]](elem: T): Build[T] =
    Build.const(elem)

  def fitToWidth[T <: Renderable[T]](elem: T, width: Double) = {
    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt
    val bounds = Bounds(0, 0, width, height)
    fitToBounds(elem, bounds)
  }
  def fitToHeight[T <: Renderable[T]](elem: T, height: Double) = {
    if (elem.bounds.h != 0) {
      val aspect = elem.bounds.w / elem.bounds.h
      val width = (height * aspect).toInt
      val bounds = Bounds(0, 0, width, height)
      fitToBounds(elem, bounds)
    } else elem
  }
  def fitToAspect[T <: Renderable[T]](
      elem: T,
      width: Double,
      height: Double
  ) = {
    fitToWidth(fitToHeight(elem, height), width)
  }

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[T],
      layout: Layout
  ): ElemList[T] = {
    val orig = members.map(_.bounds)
    val n = layout(orig)
    val transformed = n zip members map (x => fitToBounds(x._2, x._1))
    ElemList(transformed.toList)
  }

  def sequence[T <: Renderable[T], F: FC](members: Seq[T]): ElemList[T] =
    sequence(members, FreeLayout)

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[Build[T]],
      layout: Layout
  ): Build[ElemList[T]] = {
    case (Some(old), e: Event, replay) =>
      val members1 = (old.members zip members) map {
        case (old, build) =>
          build(Some(old), e,replay)
      }
      sequence(members1, layout)
    case (None, BuildEvent,_) =>
      sequence(members.map(_.build), layout)
    case _ => throw new RuntimeException("should not happen")
  }
  def either[A <: Renderable[A], B <: Renderable[B], F: FC](
      memberAB: scala.Either[Build[A], Build[B]]
  ): Build[ElemEither[A, B]] = {
    case (Some(ElemEither(scala.Left(old), tx)), e: Event,replay) =>
      ElemEither(scala.Left(memberAB.left.get(Some(old), e,replay)), tx)
    case (Some(ElemEither(scala.Right(old), tx)), e: Event,replay) =>
      ElemEither(scala.Right(memberAB.right.get(Some(old), e,replay)), tx)
    case (None, BuildEvent,_) =>
      memberAB.fold(
        a => ElemEither(scala.Left(a.build)),
        b => ElemEither(scala.Right(b.build))
      )
    case _ => throw new RuntimeException("should not happen")
  }

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[Build[T]]
  ): Build[ElemList[T]] =
    sequence(members, FreeLayout)

  def sequence2[T1 <: Renderable[T1], T2 <: Renderable[T2], F: FC](
      members: Seq[Either[T1, T2]],
      layout: Layout
  ): ElemList2[T1, T2] = {
    val bounds = members.map(_.fold(_.bounds, _.bounds))

    val n = layout(bounds)

    val transformed = n zip members map (
        x =>
          x._2 match {
            case scala.util.Left(y) =>
              scala.util.Left(fitToBounds(y, x._1))
            case scala.util.Right(y) =>
              scala.util.Right(fitToBounds(y, x._1))
          }
      )
    ElemList2(transformed)
  }

  def sequence2[T1 <: Renderable[T1], T2 <: Renderable[T2], F: FC](
      members1: Seq[Either[Build[T1], Build[T2]]],
      layout: Layout
  ): Build[ElemList2[T1, T2]] = {
    case (None, BuildEvent,_) =>
      sequence2(
        members1.map(
          _.fold(x => scala.util.Left(x.build), x => scala.util.Right(x.build))
        ),
        layout
      )
    case (Some(old), e: Event,replay) =>
      val members: Seq[Either[T1, T2]] = (old.members zip members1) map {
        case (old, build) =>
          build match {
            case scala.util.Left(x) =>
              scala.util.Left(x(Some(old.left.get) , e,replay))
            case scala.util.Right(x) =>
              scala.util.Right(x(Some(old.right.get) , e,replay))
          }
      }
      sequence2(members, layout)

    case _ => throw new RuntimeException("should not happen")
  }

  /* Normalized scientific notation. */
  def scientific(x: Double) =
    x / math.pow(10d, math.log10(x).round) -> math.log10(x).round

  def mapPoint(p: Point, from: Bounds, to: Bounds, invertY: Boolean): Point =
    if (from.w == 0 || from.h == 0) Point(0d, 0d)
    else {
      val xF = to.w / from.w
      val yF = to.h / from.h
      Point(
        math.abs(p.x - from.x) * xF + to.x,
        if (!invertY) math.abs(p.y - from.y) * yF + to.y
        else math.abs(p.y - from.maxY) * yF + to.y
      )
    }

  val lineWidth = 0.08 fts

  def scientificNotation(x: Double, precision: Int) = {
    require(precision >= 1)
    if (x.isNaN || x.isInfinity) x.toString
    else {
      val xa = math.abs(x)
      var b = 0
      val j = if (xa < 1) -1 else 1
      val lx = xa
      var done = false
      var a = 1d
      while (!done) {
        a = lx / math.pow(10, b)
        if (a >= 0.99 && a < 10.01) {
          done = true
        } else {
          b += j
        }
      }
      var P = {
        var i = 0
        var x = 1
        while (i < precision) {
          x *= 10
          i += 1
        }
        x
      }
      var rounded = math.round(a * P)
      var k = 0
      while (rounded % 10 == 0 && k < precision - 1) {
        rounded /= 10
        P /= 10
        k += 1
      }

      val bs =
        if (b > -10 && b < 0) s"-0${math.abs(b)}"
        else if (b >= 0 && b < 10) s"0${math.abs(b)}"
        else b.toString

      s"${if (x < 0) "-" else ""}${if (P == 1) rounded.toString
      else (rounded.toDouble / P).toString}e${bs}"
    }

  }

  def decimalNotation(x: Double, precision: Int) = {
    require(precision >= 1)

    if (x.isNaN || x.isInfinity) x.toString
    else if (x == 0d) "0"
    else {
      val xa = math.abs(x)
      require(xa <= 1e-4 || xa >= 1e-4)

      var P = {
        var i = 0
        var x = 1
        while (i < precision) {
          x *= 10
          i += 1
        }
        x
      }
      var rounded = math.round(xa * P)
      if (rounded >= 1.0) {
        while (rounded % 10 == 0) {
          rounded /= 10
          P /= 10
        }
      }
      val decimal = if (P == 0) xa.toLong else rounded / P
      val fractional = if (P == 0) 0 else rounded % P

      val fractionalStr = (P + fractional).toString

      s"${if (x < 0) "-" else ""}${decimal}${if (fractionalStr.length <= 1) ""
      else s".${fractionalStr.drop(1)}"}"
    }
  }

  private val precision = 4
  val defaultTickFormatter: Seq[Double] => Seq[String] =
    (worldCoordinates: Seq[Double]) => {
      if (worldCoordinates.isEmpty) Nil
      else {
        val r = worldCoordinates.map { w =>
          if ((math.abs(w) <= 1e-4 || math.abs(w) >= 1e4) && w != 0.0)
            scientificNotation(w, precision)
          else
            decimalNotation(w, precision)
        }
        r
      }
    }
}