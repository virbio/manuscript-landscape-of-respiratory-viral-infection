package org.nspl

import data._
import scala.util.Try

trait DataRenderer {
  def render[R <: RenderingContext[R]](
      data: Row,
      xAxis: Axis,
      yAxis: Axis,
      ctx: R,
      tx: AffineTransform,
      descriptor: Descriptor
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  def asLegend: List[(String, LegendElem)]
  def clear[R <: RenderingContext[R]](ctx: R)(
      implicit
      re: Renderer[ShapeElem, R],
      rt: Renderer[TextBox, R]
  ): Unit = ()
  def xMinMax(ds: DataSource): Option[MinMax]
  def yMinMax(ds: DataSource): Option[MinMax]
}

trait Renderers {

  def point[T: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      sizeCol: Int = 3,
      shapeCol: Int = 4,
      errorTopCol: Int = 5,
      errorBottomCol: Int = 6,
      labelColorCol: Int = 7,
      strokeColorCol: Int = 8,
      errorLeftCol: Int = 9,
      errorRightCol: Int = 10,
      size: Double = 3d,
      strokeWidth: Option[Double] = None,
      color: Colormap = DiscreteColors(14),
      strokeColor: Option[Colormap] = None,
      shapes: Vector[Shape] = shapeList,
      pointSizeIsInDataSpaceUnits: Boolean = false,
      keepPointShapeAspectRatio: Boolean = false,
      valueText: Boolean = false,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 0.4 fts,
      labelFontBold: Boolean = false,
      labelColor: Colormap = Color.black,
      labelConnectionLineStroke: Stroke = Stroke(lineWidth * 0.3),
      labelConnectionLineColor: Color = Color.gray5,
      errorBarStroke: Stroke = Stroke(lineWidth),
      errorBarColor: Colormap = Color.black,
      transparent: Option[Double] = None,
      translate: (Double, Double) = (0d, 0d),
      xJitterStrength: Double = 0d,
      yJitterStrength: Double = 0d,
      label: Any => String = _.toString,
      colorLegend: Seq[(String, Double)] = Nil,
      shapeLegend: Seq[String] = Nil,
      forceDirectedLabelLayout: Boolean = false,
      forceDirectedLabelLayoutSteps: Int = 10,
      maxForceDirectedLabels: Int = Int.MaxValue,
      noDescriptor: Boolean = true,
      polygonTriangles: List[(Colormap, Int)] = Nil
  ) = new DataRenderer {

    def asLegend =
      if (colorLegend.nonEmpty || shapeLegend.nonEmpty)
        colorLegend.toList.map {
          case (name, colorValue) =>
            (name, PointLegend(shapes.head, color(colorValue)))
        } ++
          shapeLegend.zipWithIndex.map {
            case (tag, shapeIdx) =>
              (tag, PointLegend(shapes(shapeIdx % shapes.size), Color.gray4))
          } else List(("", PointLegend(shapes.head, color(0))))

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = ds.columnMinMax(yCol)

    val shapesAndTextLabels =
      scala.collection.mutable
        .ArrayBuffer[(ShapeElem, TextBox, AffineTransform)]()

    override def clear[R <: RenderingContext[R]](ctx: R)(
        implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ) = {
      if (forceDirectedLabelLayout && shapesAndTextLabels.size < maxForceDirectedLabels) {
        val labelLayout =
          LabelLayout
            .many(
              shapesAndTextLabels.map(v => (v._1.bounds, v._2.bounds)).toSeq,
              forceDirectedLabelLayoutSteps
            )
        labelLayout.zip(shapesAndTextLabels).foreach {
          case ((updatedBound, connectionLine), (_, label, tx)) =>
            val labelUpdated = label
              .translate(
                updatedBound.x - label.bounds.x,
                updatedBound.y - label.bounds.y
              )
              .transform(_ => tx)

            rt.render(ctx, labelUpdated)
            connectionLine match {
              case None                                   =>
              case Some((p1, p2)) if p1.distance(p2) <= 0 =>
              case Some((p1, p2)) =>
                val lineElem = ShapeElem(
                  Shape.line(p1, p2),
                  strokeColor = labelConnectionLineColor,
                  stroke = Some(labelConnectionLineStroke)
                ).transform(_ => tx)
                re.render(ctx, lineElem)
            }
        }
        shapesAndTextLabels.clear
      } else {
        shapesAndTextLabels.foreach {
          case (_, textbox, tx) =>
            rt.render(ctx, textbox.transform(_ => tx))
        }
        shapesAndTextLabels.clear()
      }
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      if (data.dimension > xCol && data.dimension > yCol) {
        val wX = data(xCol)
        val wY = data(yCol)

        if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {
          val dataColorValue =
            if (data.dimension > colorCol) data(colorCol) else 0d
          val strokeColorValue =
            if (data.dimension > strokeColorCol) data(strokeColorCol) else 0d
          val labelColorValue =
            if (data.dimension > labelColorCol) data(labelColorCol) else 0d

          val labelColor1 = labelColor(labelColorValue)

          val errorBarColor1 = errorBarColor(dataColorValue)

          val skip = (transparent.isDefined && transparent.get == dataColorValue)

          val color1 =
            color(dataColorValue)

          val strokeColor1 = strokeColor.map(_.apply(strokeColorValue))

          if (!skip) {
            if (color1.a > 0 || strokeColor1.exists(_.a > 0)) {
              val shape =
                if (data.dimension > shapeCol)
                  shapes(data(shapeCol).toInt % shapes.size)
                else shapes.head
              val size1 = if (data.dimension > sizeCol) data(sizeCol) else size

              val unitWidthX = if (pointSizeIsInDataSpaceUnits) {
                math.abs(xAxis.worldToView(0) - xAxis.worldToView(1))
              } else 1d
              val unitWidthY = if (pointSizeIsInDataSpaceUnits) {
                math.abs(yAxis.worldToView(0) - yAxis.worldToView(1))
              } else 1d

              val shapeBounds = shape.bounds

              val shapeAspectRatio =
                if (keepPointShapeAspectRatio) shapeBounds.h / shapeBounds.w
                else 1d
              val factorX = unitWidthX * size1 / shapeBounds.w
              val factorY =
                unitWidthY * size1 * shapeAspectRatio / shapeBounds.h

              val jitterX =
                if (xJitterStrength == 0d) 0d
                else
                  ((scala.util.Random.nextDouble - 0.5) / ((xAxis.max - xAxis.min))) * xJitterStrength
              val jitterY =
                if (yJitterStrength == 0d) 0d
                else
                  ((scala.util.Random.nextDouble - 0.5) / ((yAxis.max - yAxis.min))) * yJitterStrength

              val vX = xAxis.worldToView(wX + translate._1 + jitterX)
              val vY = yAxis.worldToView(wY + translate._2 + jitterY)

              val shape1PreTransform: ShapeElem = ShapeElem(
                shape.transform(
                  _ =>
                    AffineTransform
                      .translateAndScale(vX, vY, factorX, factorY)
                ),
                fill = color1,
                strokeColor = strokeColor1.getOrElse(Color.black),
                stroke = strokeWidth.map(width => Stroke(width fts))
              )
              if (data.dimension > errorTopCol) {
                val errorTop = data(errorTopCol)
                val shape1 = ShapeElem(
                  Shape
                    .line(
                      Point(vX, vY),
                      Point(vX, yAxis.worldToView(errorTop))
                    ),
                  stroke = Some(errorBarStroke),
                  strokeColor = errorBarColor1
                ).transform(_ => tx)
                re.render(ctx, shape1)
              }
              if (data.dimension > errorBottomCol) {
                val errorBottom = data(errorBottomCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(vX, yAxis.worldToView(errorBottom))
                  ),
                  stroke = Some(errorBarStroke),
                  strokeColor = errorBarColor1
                ).transform(_ => tx)
                re.render(ctx, shape1)
              }
              if (data.dimension > errorLeftCol) {
                val error = data(errorLeftCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(xAxis.worldToView(error), vY)
                  ),
                  stroke = Some(errorBarStroke),
                  strokeColor = errorBarColor1
                ).transform(_ => tx)
                re.render(ctx, shape1)
              }
              if (data.dimension > errorRightCol) {
                val error = data(errorRightCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(xAxis.worldToView(error), vY)
                  ),
                  stroke = Some(errorBarStroke),
                  strokeColor = errorBarColor1
                ).transform(_ => tx)
                re.render(ctx, shape1)
              }
              // val shape1 = shape1PreTransform.transform(b => tx)
              re.render(
                ctx,
                shape1PreTransform
                  .transform(b => tx)
                  .withDescriptor(
                    if (noDescriptor) NoDescriptor else descriptor
                  )
              )

              if (polygonTriangles.size >= 3) {

                val radius = shapeBounds.w * factorX / 2d
                val num = polygonTriangles.length
                val angle = 2d * math.Pi / num
                polygonTriangles.zipWithIndex.foreach {
                  case ((colormap, colorCol), extraIdx) =>
                    val phi0 = angle * extraIdx
                    val phi1 = angle * (extraIdx + 1)
                    val x0 = (radius) * math.cos(phi0) + vX
                    val y0 = (radius) * math.sin(phi0) + vY
                    val x1 = (radius) * math.cos(phi1) + vX
                    val y1 = (radius) * math.sin(phi1) + vY
                    // val factorX = unitWidthX * size1 / shapeBounds.w
                    // val factorY = unitWidthY * size1 * shapeAspectRatio / shapeBounds.h
                    val dataColorValue =
                      if (data.dimension > colorCol) data(colorCol) else 0d
                    val elem = ShapeElem(
                      SimplePath(
                        List(Point(vX, vY), Point(x0, y0), Point(x1, y1))
                      ),
                      fill = colormap(dataColorValue)
                    )
                    re.render(
                      ctx,
                      elem
                        .transform(b => tx)
                        .withDescriptor(
                          if (noDescriptor) NoDescriptor else descriptor
                        )
                    )
                }
              }

              if (valueText) {
                val tbPreTransform = TextBox(
                  f"${data(colorCol)}%.2g",
                  color = labelColor1,
                  fontSize = labelFontSize,
                  bold = labelFontBold
                ).translate(vX, vY)
                  .transform(
                    b =>
                      AffineTransform
                        .translate(0, -1 * b.h - shape.bounds.h * factorY * 0.5)
                  )

                shapesAndTextLabels += (
                  (
                    shape1PreTransform,
                    tbPreTransform,
                    tx
                  )
                )

              }

              if (labelText) {
                val text = label(data.label)
                if (text != "") {
                  val tbPreTransform = TextBox(
                    text,
                    color = labelColor1,
                    fontSize = labelFontSize
                  ).translate(vX, vY)
                    .transform(
                      b =>
                        AffineTransform.translate(
                          -0.2 * b.w,
                          -1 * b.h - shape.bounds.h * factorY * 0.5
                        )
                    )

                  shapesAndTextLabels += (
                    (
                      shape1PreTransform,
                      tbPreTransform,
                      tx
                    )
                  )
                }
              }
            }
          }

        }
      } else
        throw new RuntimeException(
          s"Record has no X or Y elements. size: ${data.dimension} vs idx $xCol $yCol"
        )
    }
  }

  def line(
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      stroke: Stroke = Stroke(lineWidth),
      color: Colormap = Color.black
  ) = new DataRenderer {

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = ds.columnMinMax(yCol)

    var currentPoint: Option[Point] = None
    def asLegend = List("" -> LineLegend(stroke, color(0)))

    override def clear[R <: RenderingContext[R]](ctx: R)(
        implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = { currentPoint = None }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val p = Point(vX, vY)

        if (currentPoint.isEmpty) {
          currentPoint = Some(p)
        } else {

          val shape1 = ShapeElem(
            Shape.line(currentPoint.get, p),
            fill = Color.transparent,
            strokeColor = color1,
            stroke = Some(stroke.copy(cap = CapRound))
          ).transform(_ => tx)

          re.render(ctx, shape1)

          currentPoint = Some(p)

        }
      }
    }
  }

  /**
    *Paints the area between the (x,y) and (x,0) or
    *  between (x,y) and (x,y2) if y2 is present
    */
  def area(
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      yCol2: Option[Int] = None,
      color: Colormap = Color.black
  ) = new DataRenderer {
    var currentPoint1: Option[Point] = None
    var currentPoint2: Option[Point] = None
    def asLegend = List("" -> PointLegend(shapeList(1), color(0)))

    override def clear[R <: RenderingContext[R]](ctx: R)(
        implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      currentPoint2 = None
      currentPoint1 = None
    }

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = {
      val max = ds.columnMinMax(yCol).map(_.max)
      val min = yCol2.flatMap(y => ds.columnMinMax(y).map(_.min)).getOrElse(0d)
      max.map(max => MinMaxImpl(min, max))
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val wYBottom = yCol2.map { i =>
          val w = data(i)
          if (w > yAxis.max) yAxis.max
          else if (w < yAxis.min) yAxis.min
          else w
        }

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val p1 = Point(vX, vY)
        val p2 = Point(
          vX,
          wYBottom
            .map(w => yAxis.worldToView(w))
            .getOrElse(yAxis.worldToView(yAxis.min))
        )

        if (currentPoint1.isEmpty) {
          currentPoint1 = Some(p1)
          currentPoint2 = Some(p2)
        } else {

          val shape = SimplePath(
            List(
              Point(currentPoint1.get.x - .5, currentPoint1.get.y),
              Point(p1.x, p1.y),
              Point(p2.x, p2.y),
              Point(currentPoint2.get.x - .5, currentPoint2.get.y)
            )
          )

          val shape1 = ShapeElem(
            shape,
            fill = color1
          ).transform(_ => tx)

          re.render(ctx, shape1)

          currentPoint1 = Some(p1)
          currentPoint2 = Some(p2)

        }
      }
    }
  }

  def polynom(
      renderer: () => DataRenderer = () => line()
  ) = new DataRenderer {
    def asLegend = renderer().asLegend

    def evaluatePolynomial(coef: Array[Double], x: Double) = {
      var p = 0.0;
      var i = coef.length - 1
      while (i >= 0) {
        p = coef(i) + x * p
        i = i - 1
      }
      p
    }

    def xMinMax(ds: DataSource): Option[MinMax] = None
    def yMinMax(ds: DataSource): Option[MinMax] = None

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val r = renderer()
      val coeffs = data.allColumns.toArray
      if (coeffs.length == 2 && coeffs(1) == Double.PositiveInfinity) {
        // vertical line
        val x = coeffs(0)
        val y1 = yAxis.min
        val y2 = yAxis.max
        if (x >= xAxis.min && x < xAxis.max) {
          r.render(VectorRow(Vector(x, y1), ""), xAxis, yAxis, ctx, tx, d)
          r.render(VectorRow(Vector(x, y2), ""), xAxis, yAxis, ctx, tx, d)
        }
      } else {
        0 until 50 foreach { i =>
          val x = xAxis.min + i * (xAxis.max - xAxis.min) / 50.0
          val y = evaluatePolynomial(coeffs, x)
          r.render(VectorRow(Vector(x, y), ""), xAxis, yAxis, ctx, tx, d)
        }
      }

    }
  }

  def bar[T: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      fillCol: Int = 2,
      horizontal: Boolean = false,
      stroke: Stroke = Stroke(lineWidth),
      strokeColor: Color = Color.black,
      fill: Colormap = Color.white,
      width: Double = 1,
      yCol2: Option[Int] = None,
      widthCol: Int = 3,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 1 fts,
      labelFontConfig: Option[FontConfiguration] = None,
      labelColor: Colormap = Color.black,
      fitLabelToBox: Boolean = false
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] =
      if (horizontal) ds.columnMinMax(xCol)
      else {
        val bounds = ds.iterator.map { row =>
          val mid = row(xCol)
          val w =
            if (row.allColumns.size > widthCol) row(widthCol) else width
          (mid - w, mid + w)
        }.toList
        if (bounds.isEmpty) None
        else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
      }
    def yMinMax(ds: DataSource): Option[MinMax] =
      if (!horizontal) ds.columnMinMax(yCol)
      else {
        val bounds = ds.iterator.map { row =>
          val mid = row(yCol)
          val w =
            if (row.allColumns.size > widthCol) row(widthCol) else width
          (mid - w, mid + w)
        }.toList
        if (bounds.isEmpty) None
        else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
      }

    def asLegend = List("" -> PointLegend(shapeList(1), fill(0)))
    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 =
          if (data.dimension > fillCol) fill(data(fillCol)) else fill(0d)
        val labelColor1 =
          if (data.dimension > fillCol) labelColor(data(fillCol))
          else labelColor(0d)
        val width1 = if (data.dimension > widthCol) data(widthCol) else width

        if (!horizontal) {

          val wYBottom = yCol2
            .map { i =>
              val w = data(i)
              if (w > yAxis.max) yAxis.max
              else if (w < yAxis.min) yAxis.min
              else w
            }
            .getOrElse(
              if (0d > yAxis.max) yAxis.max
              else if (0d < yAxis.min) yAxis.min
              else 0d
            )

          val vX = xAxis.worldToView(wX)
          val vXMin = xAxis.worldToView(xAxis.min)
          val vXMax = xAxis.worldToView(xAxis.max)
          val vWidth1 =
            math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))

          val outOfBoundsLeft = math.max(0d, vXMin - (vX - vWidth1 * 0.5))
          val outOfBoundsRight = math.max(0d, vX + vWidth1 * 0.5 - vXMax)

          val vWidth = vWidth1 - outOfBoundsLeft - outOfBoundsRight

          val vY2 = yAxis.worldToView(wYBottom)
          val vY = yAxis.worldToView(wY)

          val vHeight = math.abs(vY2 - vY)

          val rectangle =
            if (vY2 > vY)
              Shape.rectangle(
                vX - vWidth1 * 0.5 + outOfBoundsLeft,
                vY,
                vWidth,
                vHeight
              )
            else
              Shape.rectangle(
                vX - vWidth1 * 0.5 + outOfBoundsLeft,
                vY2,
                vWidth,
                vHeight
              )

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = if (stroke.width == 0d.fts) None else Some(stroke),
            strokeColor = strokeColor
          ).transform(_ => tx)

          re.render(ctx, shape1)

          if (labelText) {
            val fc =
              if (labelFontConfig.isEmpty) implicitly[FontConfiguration]
              else labelFontConfig.get
            val textBox = TextBox(
              data.label,
              Point(
                rectangle.bounds.centerX - rectangle.bounds.w * 0.25,
                rectangle.bounds.centerY
              ),
              fontSize = labelFontSize,
              color = labelColor1
            )(fc).transform(_ => tx)
            val scaled = if (fitLabelToBox) {
              fitToBounds(textBox, shape1.bounds)
            } else textBox
            rt.render(ctx, scaled)
          }

        } else {

          val wXBottom = yCol2
            .map { i =>
              val w = data(i)
              if (w > xAxis.max) xAxis.max
              else if (w < xAxis.min) xAxis.min
              else w
            }
            .getOrElse(
              if (0d > xAxis.max) xAxis.max
              else if (0d < xAxis.min) xAxis.min
              else 0d
            )

          val vY = yAxis.worldToView(wY)
          val vYMin = yAxis.worldToView(yAxis.min)
          val vYMax = yAxis.worldToView(yAxis.max)

          val vWidth1 =
            math.abs(yAxis.worldToView(0.0) - yAxis.worldToView(width))

          val outOfBoundsTop = math.max(0d, vYMax - (vY - vWidth1 * 0.5))
          val outOfBoundsBottom = math.max(0d, vY + vWidth1 * 0.5 - vYMin)

          val vWidth = vWidth1 - outOfBoundsTop - outOfBoundsBottom

          val vX = xAxis.worldToView(wX)
          val vX2 = xAxis.worldToView(wXBottom)
          val vHeight = math.abs(vX2 - vX)

          val rectangle =
            if (wX > 0)
              Shape.rectangle(
                vX2,
                vY - vWidth1 * 0.5 + outOfBoundsTop,
                vHeight,
                vWidth
              )
            else
              Shape.rectangle(
                vX,
                vY - vWidth1 * 0.5 + outOfBoundsTop,
                vHeight,
                vWidth
              )

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = if (stroke.width == 0d.fts) None else Some(stroke),
            strokeColor = strokeColor
          ).transform(_ => tx)

          re.render(ctx, shape1)

          if (labelText) {
            val fc =
              if (labelFontConfig.isEmpty) implicitly[FontConfiguration]
              else labelFontConfig.get

            val textBox = TextBox(
              data.label,
              Point(
                rectangle.bounds.centerX - rectangle.bounds.w * 0.25,
                rectangle.bounds.centerY
              ),
              fontSize = labelFontSize,
              color = labelColor1
            )(fc).transform(_ => tx)
            val scaled = if (fitLabelToBox) {
              fitToBounds(textBox, shape1.bounds)
            } else textBox
            rt.render(ctx, scaled)

          }

        }

      }
    }
  }

  def boxwhisker(
      xCol: Int = 0,
      medianCol: Int = 1,
      q1Col: Int = 2,
      q3Col: Int = 3,
      minCol: Int = 4,
      maxCol: Int = 5,
      widthCol: Int = 6,
      fillCol: Int = 7,
      width: Double = 1,
      stroke: Stroke = Stroke(lineWidth),
      strokeColor: Color = Color.black,
      fill: Colormap = Color.white
  ) = new DataRenderer {
    def asLegend = List("" -> PointLegend(shapeList(1), fill(0)))

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val bounds = ds.iterator.map { row =>
        val mid = row(xCol)
        val w =
          if (row.allColumns.size > widthCol) row(widthCol) else width
        (mid - w, mid + w)
      }.toList
      if (bounds.isEmpty) None
      else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
    }
    def yMinMax(ds: DataSource): Option[MinMax] = {
      val min = ds.columnMinMax(minCol).map(_.min)
      val max = ds.columnMinMax(maxCol).map(_.max)
      for {
        min <- min
        max <- max
      } yield MinMaxImpl(min, max)
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX1 = data(xCol)
      val q2 = data(medianCol)
      val q1 = data(q1Col)
      val q3 = data(q3Col)
      val min = data(minCol)
      val max = data(maxCol)
      val color1 =
        if (data.dimension > fillCol) fill(data(fillCol)) else fill(wX1)
      val width1 = if (data.dimension > widthCol) data(widthCol) else width
      val wX = wX1

      if (wX >= xAxis.min && wX <= xAxis.max) {

        val vWidth =
          math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))
        val vX = xAxis.worldToView(wX)
        val vQ1 = yAxis.worldToView(q1)
        val vQ2 = yAxis.worldToView(q2)
        val vQ3 = yAxis.worldToView(q3)
        val vMin = yAxis.worldToView(min)
        val vMax = yAxis.worldToView(max)
        val vHeight = math.abs(vQ1 - vQ3)

        val yAxisInverted = {
          yAxis.worldToView(1.0) < yAxis.worldToView(0.5)
        }

        val shape1 =
          if (yAxisInverted)
            ShapeElem(
              Shape.rectangle(vX - vWidth * 0.5, vQ3, vWidth, vHeight),
              fill = color1,
              stroke = Some(stroke),
              strokeColor = strokeColor
            ).transform(_ => tx)
          else
            ShapeElem(
              Shape.rectangle(vX - vWidth * 0.5, vQ1, vWidth, vHeight),
              fill = color1,
              stroke = Some(stroke),
              strokeColor = strokeColor
            ).transform(_ => tx)

        re.render(ctx, shape1)

        val shape2 = ShapeElem(
          Shape
            .line(Point(vX - vWidth * 0.5, vQ2), Point(vX + vWidth * 0.5, vQ2)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape2)

        val shape3 = ShapeElem(
          Shape.line(Point(vX, vQ1), Point(vX, vMin)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape3)

        val shape4 = ShapeElem(
          Shape.line(Point(vX, vQ3), Point(vX, vMax)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape4)

      }
    }
  }

  def lineSegment[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      x2Col: Int = 2,
      y2Col: Int = 3,
      colorCol: Int = 4,
      offsetCol: Int = 5,
      strokeCol: Int = 6,
      strokes: List[Stroke] = List(Stroke(lineWidth, CapRound)),
      color: Colormap = HeatMapColors(0, 1),
      labelText: Boolean = false,
      labelColor: Color = Color.black,
      labelFontSize: RelFontSize = 0.4 fts,
      labelDistance: RelFontSize = 0.4 fts,
      label: Any => String = _.toString,
      arrowForward: Boolean = false,
      arrowBackward: Boolean = false,
      arrowSize: RelFontSize = 0.2 fts
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(xCol)
      val m2 = ds.columnMinMax(x2Col)
      for {
        m1 <- m1
        m2 <- m2
      } yield MinMaxImpl(math.min(m1.min, m2.min), math.max(m1.max, m2.max))
    }

    def yMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(yCol)
      val m2 = ds.columnMinMax(y2Col)
      for {
        m1 <- m1
        m2 <- m2
      } yield MinMaxImpl(math.min(m1.min, m2.min), math.max(m1.max, m2.max))
    }

    def asLegend = List("" -> LineLegend(strokes.head, color(0)))

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)
      val wX2 = data(x2Col)
      val wY2 = data(y2Col)

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)
        val stroke1 =
          if (data.dimension > strokeCol) strokes(data(strokeCol).toInt) else strokes(0)

        val wXInBound = math.max(math.min(xAxis.max,wX),xAxis.min)
        val wX2InBound = math.max(math.min(xAxis.max,wX2),xAxis.min)

        val wYInBound = math.max(math.min(yAxis.max,wY),yAxis.min)
        val wY2InBound = math.max(math.min(yAxis.max,wY2),yAxis.min)

        val vX = xAxis.worldToView(wXInBound)
        val vY = yAxis.worldToView(wYInBound)

        val vX2 = xAxis.worldToView(wX2InBound)
        val vY2 = yAxis.worldToView(wY2InBound)

        val (offsetX, offsetY) = if (data.dimension > offsetCol) {
          val v0 = vX - vX2
          val v1 = vY - vY2
          val nd0 = v1
          val nd1 = -1 * v0
          val l = math.sqrt(nd0 * nd0 + nd1 * nd1)
          val n0 = nd0 / l
          val n1 = nd1 / l

          val shift = data(offsetCol) * stroke1.width.value
          (n0 * shift, n1 * shift)
        } else (0d, 0d)

        val p = Point(vX + offsetX, vY + offsetY)
        val p2 = Point(vX2 + offsetX, vY2 + offsetY)

        val shape1 = ShapeElem(
          Shape.line(p, p2),
          strokeColor = color1,
          stroke = Some(stroke1)
        ).transform(_ => tx)

        re.render(ctx, shape1)

        val arrowSizeV = math.min(arrowSize.value, p.distance(p2))
        if (arrowForward) {
          val (dx, dy) = p2.directionTo(p)
          val arrowBase = {
            p2.translate(dx * arrowSizeV, dy * arrowSizeV)
          }
          val normal1 = (dy, -dx)
          val normal2 = (-dy, dx)
          val vertex1 =
            arrowBase.translate(
              normal1._1 * arrowSizeV * 0.5,
              normal1._2 * arrowSizeV * 0.5
            )
          val vertex2 =
            arrowBase.translate(
              normal2._1 * arrowSizeV * 0.5,
              normal2._2 * arrowSizeV * 0.5
            )
          val shape = ShapeElem(
            SimplePath(
              List(p2, vertex1, vertex2)
            ),
            strokeColor = color1,
            stroke = Some(stroke1),
            fill = color1
          ).transform(_ => tx)
          re.render(ctx, shape)
        }
        if (arrowBackward) {
          val (dx, dy) = p.directionTo(p2)
          val arrowBase = {
            p.translate(dx * arrowSizeV, dy * arrowSizeV)
          }
          val normal1 = (dy, -dx)
          val normal2 = (-dy, dx)
          val vertex1 =
            arrowBase.translate(
              normal1._1 * arrowSizeV * 0.5,
              normal1._2 * arrowSizeV * 0.5
            )
          val vertex2 =
            arrowBase.translate(
              normal2._1 * arrowSizeV * 0.5,
              normal2._2 * arrowSizeV * 0.5
            )
          val shape = ShapeElem(
            SimplePath(
              List(p, vertex1, vertex2)
            ),
            strokeColor = color1,
            stroke = Some(stroke1),
            fill = color1
          ).transform(_ => tx)
          re.render(ctx, shape)
        }

        if (labelText) {
          val midPointX = (vX + vX2) * 0.5
          val midPointY = (vY + vY2) * 0.5
          val (normalX, normalY) = {
            val aX = vX2 - vX
            val aY = vY2 - vY
            val length = math.sqrt(aX * aX + aY * aY)
            (-1 * aY / length, aX / length)
          }
          val labelX = midPointX + labelDistance.value * normalX
          val labelY = midPointY + labelDistance.value * normalY

          val tb = TextBox(
            label(data.label),
            color = labelColor,
            fontSize = labelFontSize
          ).translate(labelX, labelY)
            .transform(
              b => tx.concat(AffineTransform.translate(-1 * b.w * 0.5, 0.0))
            )

          rt.render(ctx, tb)
        }

      }
    }
  }

  def cubicBezier(
      xCol: Int = 0,
      yCol: Int = 1,
      x2Col: Int = 2,
      y2Col: Int = 3,
      x3Col: Int = 4,
      y3Col: Int = 5,
      x4Col: Int = 6,
      y4Col: Int = 7,
      colorCol: Int = 8,
      stroke: Stroke = Stroke(lineWidth),
      color: Colormap = HeatMapColors(0, 1)
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(xCol)
      val m2 = ds.columnMinMax(x2Col)
      val m3 = ds.columnMinMax(x3Col)
      val m4 = ds.columnMinMax(x4Col)
      val l = List(m1, m2, m3, m4).flatMap(_.toSeq)
      if (l.isEmpty) None
      else {
        val min = l.minBy(_.min).min
        val max = l.maxBy(_.max).max
        Some(MinMaxImpl(min, max))
      }
    }

    def yMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(yCol)
      val m2 = ds.columnMinMax(y2Col)
      val m3 = ds.columnMinMax(y3Col)
      val m4 = ds.columnMinMax(y4Col)
      val l = List(m1, m2, m3, m4).flatMap(_.toSeq)
      if (l.isEmpty) None
      else {
        val min = l.minBy(_.min).min
        val max = l.maxBy(_.max).max
        Some(MinMaxImpl(min, max))
      }
    }

    def asLegend = List("" -> LineLegend(stroke, color(0)))

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        d: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX1 = data(xCol)
      val wY1 = data(yCol)
      val wX2 = data(x2Col)
      val wY2 = data(y2Col)
      val wX3 = data(x3Col)
      val wY3 = data(y3Col)
      val wX4 = data(x4Col)
      val wY4 = data(y4Col)

      val color1 =
        if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

      val shape1 = ShapeElem(
        Path(
          List(
            MoveTo(Point(xAxis.worldToView(wX1), yAxis.worldToView(wY1))),
            CubicTo(
              Point(xAxis.worldToView(wX4), yAxis.worldToView(wY4)),
              Point(xAxis.worldToView(wX2), yAxis.worldToView(wY2)),
              Point(xAxis.worldToView(wX3), yAxis.worldToView(wY3))
            ),
            MoveTo(Point(xAxis.worldToView(wX1), yAxis.worldToView(wY1)))
          ),
          AffineTransform.identity
        ),
        strokeColor = color1,
        fill = Color.transparent,
        stroke = Some(stroke.copy(cap = CapRound))
      ).transform(_ => tx)

      re.render(ctx, shape1)
    }
  }

}
