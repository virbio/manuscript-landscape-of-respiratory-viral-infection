package org.nspl

trait Axis {
  def worldToView(v: Double): Double
  def viewToWorld(v: Double): Double
  def min: Double
  def max: Double
  def width = math.abs(worldToView(max) - worldToView(min))
  def horizontal: Boolean
  def isLog10: Boolean
  def isLog2: Boolean
}

sealed trait AxisFactory {
  def make(
      min: Double,
      max: Double,
      width: Double,
      horizontal: Boolean,
      inverted: Boolean
  ): Axis
}

object LinearAxisFactory extends AxisFactory {
  def make(
      min1: Double,
      max1: Double,
      width1: Double,
      horizontal: Boolean,
      inverted: Boolean
  ) = {
    val h = horizontal
    if (!inverted)
      new Axis {
        def worldToView(v: Double) = ((v - min1) / (max1 - min1)) * width1
        def viewToWorld(x: Double) = (((x / width1) * (max1 - min1)) + min1)
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = h
        val isLog2 = false
        val isLog10 = false
      } else
      new Axis {
        def worldToView(v: Double) =
          width1 - ((v - min1) / (max1 - min1)) * width1
        def viewToWorld(x: Double) =
          ((((x - width1) * (-1)) / width1) * (max1 - min1)) + min1
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = h
        val isLog2 = false
        val isLog10 = false
      }
  }
}

object Log10AxisFactory extends AxisFactory {
  def make(
      min1: Double,
      max1: Double,
      width1: Double,
      horizontal: Boolean,
      inverted: Boolean
  ) = {
    val lMin1 = math.log10(min1)
    val lMax1 = math.log10(max1)
    if (horizontal)
      new Axis {
        def viewToWorld(x: Double) = ???
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          (math.log10(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = true
        val isLog2 = false
        val isLog10 = true
      } else
      new Axis {
        def viewToWorld(x: Double) = ???
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          width1 - (math.log10(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = false
        val isLog2 = false
        val isLog10 = true
      }
  }
}
object Log2AxisFactory extends AxisFactory {
  def make(
      min1: Double,
      max1: Double,
      width1: Double,
      horizontal: Boolean,
      inverted: Boolean
  ) = {
    def log2(x: Double) = math.log10(x) / math.log10(2d)
    val lMin1 = log2(min1)
    val lMax1 = log2(max1)
    if (horizontal)
      new Axis {
        def viewToWorld(x: Double) = ???
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          (log2(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = true
        val isLog2 = true
        val isLog10 = false
      } else
      new Axis {
        def viewToWorld(x: Double) = ???
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          width1 - (log2(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = false
        val isLog2 = true
        val isLog10 = false
      }
  }
}

case class AxisSettings(
    axisFactory: AxisFactory,
    numTicks: Int = 4,
    tickSpace: Option[Double] = None,
    baseTick: Option[Double] = None,
    numMinorTicksFactor: Int = 5,
    tickLength: RelFontSize = .35 fts,
    tickLabelDistance: RelFontSize = 0.5 fts,
    customTicks: Seq[(Double, AttributedString)] = Nil,
    labelRotation: Double = 0,
    width: RelFontSize = 20 fts,
    fontSize: RelFontSize = 1 fts,
    tickAlignment: Double = -1.0,
    lineWidth: RelFontSize = lineWidth,
    lineLengthFraction: Double = 1d,
    lineStartFraction: Double = 0.0,
    tickFormatter: Seq[Double] => Seq[String] = defaultTickFormatter,
    forceMajorTickOnMin: Boolean = false,
    forceMajorTickOnMax: Boolean = false,
    visible: Boolean = true,
    tickLabelColors: Double => Option[Color] = _ => None,
    dropLabelIfOutside: Boolean = false
)(implicit fc: FontConfiguration) {

  def renderable(
      axis: Axis,
      noTickLabel: Boolean,
      disableTicksAt: List[Double] = Nil
  ): (List[Double], List[Double], AxisElem) = {

    import axis._

    val horizontal = axis.horizontal

    val tickSpace1 = tickSpace.getOrElse {
      (axis.max - axis.min) / (numTicks)
    }

    val numTicks1 =
      if (tickSpace.isEmpty) numTicks else (axis.max - axis.min) / tickSpace1

    val lineStart = lineStartFraction * axis.width
    val lineEnd = lineStart + axis.width * lineLengthFraction

    val line =
      if (horizontal)
        ShapeElem(
          Shape.line(Point(lineStart, 0d), Point(lineEnd, 0d)),
          stroke = Some(Stroke(lineWidth))
        )
      else
        ShapeElem(
          Shape.line(Point(0d, lineStart), Point(0d, lineEnd)),
          stroke = Some(Stroke(lineWidth))
        )

    val (axisViewStart, axisViewEnd) =
      if (horizontal) (line.bounds.x, line.bounds.maxX)
      else (line.bounds.y, line.bounds.maxY)

    val (majorTicks1, minorTicks1) = if (axis.isLog10) {
      val lmaj1 =
        (math
          .log10(axis.min)
          .ceil
          .toInt until math
          .log10(axis.max)
          .toInt)
          .map(_.toDouble)
          .filter { i =>
            val e = math.pow(10d, i)
            e >= axis.min - 1e-3 && e <= axis.max + 1e-3
          }
      val majorTicksExp = axis.min +: (lmaj1.map(i => math.pow(10d, i)) :+ axis.max)
      val minorTicksExp = majorTicksExp
        .sliding(2)
        .flatMap { group =>
          val m1 = group(0)
          val m2 = group(1)
          val space = (m2 - m1) / numMinorTicksFactor
          (0 to numMinorTicksFactor.toInt).map(i => m1 + i * space)
        }
        .filterNot(majorTicksExp.contains)
        .toList
      (majorTicksExp, minorTicksExp)
    } else if (axis.isLog2) {
      def log2(x: Double) =
        math
          .log10(x) / math.log10(2d)

      val lmaj1 =
        (log2(axis.min).ceil.toInt until log2(axis.max).toInt)
          .map(_.toDouble)
          .filter { i =>
            val e = math.pow(2d, i)
            e >= axis.min - 1e-3 && e <= axis.max + 1e-3
          }
      val majorTicksExp = axis.min +: (lmaj1.map(i => math.pow(2d, i)) :+ axis.max)
      val minorTicksExp = majorTicksExp
        .sliding(2)
        .flatMap { group =>
          val m1 = group(0)
          val m2 = group(1)
          val space = (m2 - m1) / numMinorTicksFactor
          (0 to numMinorTicksFactor.toInt).map(i => m1 + i * space)
        }
        .filterNot(majorTicksExp.contains)
        .toList
      (majorTicksExp, minorTicksExp)
    } else
      Ticks.heckbert(axis.min, axis.max, numTicks1.toInt, numMinorTicksFactor)

    val majorTicks =
      if (numTicks1 == 0) Nil
      else {
        val addMin =
          if (forceMajorTickOnMin && !majorTicks1.contains(axis.min))
            List(axis.min)
          else Nil
        val addMax =
          if (forceMajorTickOnMax && !majorTicks1.contains(axis.max))
            List(axis.max)
          else Nil

        (majorTicks1 ++ addMin ++ addMax).iterator
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filterNot(x => disableTicksAt.contains(x))
          .filter(w => w <= axis.max && w >= axis.min)
          .toList
          .distinct
      }

    def makeTick(
        world: Double,
        text: AttributedString,
        availableSpace: Double,
        color: Color
    ): Elems2[ShapeElem, ElemOption[TextBox]] = {
      val view = worldToView(world)
      if (horizontal) {
        val textbox: ElemOption[TextBox] = {
          val tb = transform(
            transform(
              transform(
                TextBox(
                  text.text,
                  Point(view, 0.0),
                  fontSize = fontSize,
                  width =
                    if (labelRotation == 0.0) Some(availableSpace)
                    else None,
                  color = color,
                  bold = text.bold,
                  oblique = text.oblique,
                  superScript = text.superScript,
                  subScript = text.subScript,
                  underline = text.underline
                ),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x, b.centerY)
              ),
              (b: Bounds) =>
                AffineTransform.translate(0, tickLabelDistance.value)
            ),
            (b: Bounds) =>
              AffineTransform
                .translate(if (labelRotation == 0.0) b.w * (-0.5) else 0, 0)
          )
          if (!dropLabelIfOutside || tb.bounds.x > axisViewStart && tb.bounds.maxX < axisViewEnd)
            ElemOption(Some(tb))
          else ElemOption(None)
        }
        val tickLine = ShapeElem(
          Shape
            .line(
              Point(view, 0d),
              Point(view, tickAlignment * tickLength.value)
            ),
          stroke = Some(Stroke(lineWidth))
        )

        group(
          tickLine,
          textbox,
          FreeLayout
        )
      } else {

        val textbox: ElemOption[TextBox] = {
          val tb = transform(
            transform(
              transform(
                TextBox(
                  text.text,
                  Point(0.0, view),
                  fontSize = fontSize,
                  color = color,
                  bold = text.bold,
                  oblique = text.oblique,
                  superScript = text.superScript,
                  subScript = text.subScript,
                  underline = text.underline
                ),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x + b.w, b.centerY)
              ),
              (b: Bounds) =>
                AffineTransform.translate(-1 * tickLabelDistance.value - b.w, 0)
            ),
            (b: Bounds) =>
              AffineTransform
                .translate(0, if (labelRotation == 0.0) b.h * (-0.5) else 0)
          )
          if (!dropLabelIfOutside || tb.bounds.y > axisViewStart && tb.bounds.maxY < axisViewEnd)
            ElemOption(Some(tb))
          else ElemOption(None)
        }
        val tickLine = ShapeElem(
          Shape.line(
            Point(0d, view),
            Point(-1 * tickAlignment * tickLength.value, view)
          ),
          stroke = Some(Stroke(lineWidth))
        )
        group(
          tickLine,
          textbox,
          FreeLayout
        )
      }
    }

    def makeMinorTick(world: Double) = {
      val view = worldToView(world)
      if (horizontal)
        ShapeElem(
          Shape.line(
            Point(view, 0d),
            Point(view, tickLength.value * 0.5 * tickAlignment)
          ),
          stroke = Some(Stroke(lineWidth))
        )
      else
        ShapeElem(
          Shape.line(
            Point(0d, view),
            Point(-1 * tickLength.value * 0.5 * tickAlignment, view)
          ),
          stroke = Some(Stroke(lineWidth))
        )
    }

    val extra =
      customTicks
        .filter(i => i._1 >= axis.min && i._1 <= axis.max)

    val minorTicks =
      if (numTicks1 == 0 || numMinorTicksFactor <= 0) Nil
      else
        minorTicks1.iterator
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filter(w => w <= axis.max && w >= axis.min)
          .filterNot(majorTicks.contains)
          .toList
          .distinct

    val majorTickLabels =
      // if (isLog2) tickFormatter(majorTicks.map(x => math.log10(x) / math.log10(2d)))
      // else if (isLog10) tickFormatter(majorTicks.map(math.log10))
      // else
      tickFormatter(majorTicks)

    val majorTickElems = sequence(
      {
        val allMajorTicks = ((majorTicks zip majorTickLabels.map(AttributedString.apply)) ++ extra)
          .sortBy(_._1)

        val allMajorTicksWithAvailableSpace =
          (List(scala.util.Left(axis.min)) ++ allMajorTicks.map(
            scala.util.Right(_)
          ) ++
            List(scala.util.Left(axis.max))).sliding(3).toList.flatMap {
            group =>
              if (group.find(_.isRight).isEmpty) Nil
              else {
                val leftWorld = group(0).fold(identity, _._1)
                val middleWorld = group(1).right.get._1
                val rightWorld = group(2).fold(identity, _._1)
                val (world, tick) = group(1).right.get
                val color = tickLabelColors(world).getOrElse(Color.black)
                val leftV = axis.worldToView(leftWorld)
                val middleV = axis.worldToView(middleWorld)
                val rightV = axis.worldToView(rightWorld)
                val availableHorizontalSpace =
                  (middleV - leftV) * 0.5 + (rightV - middleV) * 0.5
                List((world, tick, availableHorizontalSpace, color))
              }
          }

        allMajorTicksWithAvailableSpace.map {
          case (world, text, availableSpace, color) =>
            makeTick(
              world,
              if (noTickLabel) AttributedString("") else text,
              availableSpace,
              color
            )
        }
      }
    )

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    (
      majorTicks,
      customTicks.map(_._1).toList,
      group(line, majorTickElems, minorTickElems, FreeLayout)
    )

  }
}
