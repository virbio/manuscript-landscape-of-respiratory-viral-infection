package org.nspl

import data._

case class DataElem(
    data: DataSource,
    xAxis: Axis,
    yAxis: Axis,
    renderers: Seq[DataRenderer],
    originalBounds: Bounds,
    tx: AffineTransform = AffineTransform.identity,
    idx: DataSourceIdx
) extends Renderable[DataElem] {
  def transform(tx: Bounds => AffineTransform) = {
    val ntx = tx(bounds).concat(this.tx)
    this.copy(tx = ntx)
  }
  def bounds = tx.transform(originalBounds)
}

object DataElem {
  implicit def dataElemRenderer[RC <: RenderingContext[RC]](
      implicit
      re: Renderer[ShapeElem, RC],
      rt: Renderer[TextBox, RC]
  ) = new Renderer[DataElem, RC] {
    def render(r: RC, e: DataElem): Unit = {
      e.data.iterator.zipWithIndex.foreach {
        case (row, rowIdx) =>
          e.renderers.foreach {
            case dr =>
              dr.render(
                row,
                e.xAxis,
                e.yAxis,
                r,
                e.tx,
                DataRowIdx(e.idx.externalDataSourceIdx, e.idx.i, rowIdx)
              )
          }
      }
      e.renderers.foreach(_.clear(r))
    }
  }
}
case class DelayedElem(
    delayedRenderFn: DelayedElem.Render,
    originalBounds: Bounds,
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[DelayedElem] {
  def transform(tx: Bounds => AffineTransform) = {
    val ntx = tx(bounds).concat(this.tx)
    this.copy(tx = ntx)
  }
  def bounds = tx.transform(originalBounds)
}

object DelayedElem {

  trait Render {
    def apply[R <: RenderingContext[R]](
        ctx: R,
        bounds: Bounds
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  }

  implicit def renderer[RC <: RenderingContext[RC]](
      implicit re: Renderer[ShapeElem, RC],
      rt: Renderer[TextBox, RC]
  ) = new Renderer[DelayedElem, RC] {
    def render(r: RC, e: DelayedElem): Unit = {
      e.delayedRenderFn(r, e.bounds)
    }
  }
}

trait Plots {

  // format: off
  type XYPlotAreaType = 
    Elems3[
      ShapeElem,
      Elems3[
        ShapeElem,
        Elems2[
          Elems2[Elems2[Elems2[
            Elems6[ElemList[ShapeElem],ElemList[ShapeElem],ElemList[DataElem],Elems2[AxisElem,AxisElem],ShapeElem, ElemOption[ShapeElem]], 
            TextBox],TextBox],TextBox],
          ElemOption[Elems2[ElemList[DataElem],ElemOption[AxisElem]]]
        ],
        ShapeElem
      ],
      ShapeElem
    ]
  // format: on
  case class XYPlotArea(
      elem: Option[XYPlotAreaType],
      xMin: Double,
      xMax: Double,
      yMin: Double,
      yMax: Double
  ) extends Renderable[XYPlotArea] {
    def transform(v: Bounds => AffineTransform) =
      this.copy(elem = elem.map(_.transform(v)))
    def bounds: Bounds = elem.map(_.bounds).getOrElse(Bounds.apply(0d,0d,0d,0d))
  }

  object XYPlotArea {
    implicit def renderer[RC <: RenderingContext[RC]](
        implicit re: Renderer[ShapeElem, RC],
        rt: Renderer[TextBox, RC]
    ) = new Renderer[XYPlotArea, RC] {
      def render(r: RC, e: XYPlotArea): Unit =
        e.elem.foreach(e => implicitly[Renderer[XYPlotAreaType, RC]].render(r, e))
    }
  }

  def xyplotareaBuild[F: FC](
    areaId: Option[String],
      data: Seq[(DataSource, List[DataRenderer])],
      externalDataSourceIdx: Int,
      xAxisSetting: AxisSettings,
      yAxisSetting: AxisSettings,
      origin: Option[Point] = None,
      xlim: Option[(Double, Double)] = None,
      ylim: Option[(Double, Double)] = None,
      xAxisMargin: Double = 0.05,
      yAxisMargin: Double = 0.05,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      frame: Boolean = true,
      xCustomGrid: Boolean = false,
      yCustomGrid: Boolean = false,
      main: String = "",
      mainFontSize: RelFontSize = 1.2 fts,
      mainDistance: RelFontSize = 1.2 fts,
      xlab: String = "",
      xlabFontSize: RelFontSize = 1.0 fts,
      xlabDistance: RelFontSize = 0.5 fts,
      xlabAlignment: Alignment = Center,
      ylab: String = "",
      ylabFontSize: RelFontSize = 1.0 fts,
      ylabDistance: RelFontSize = 0.5 fts,
      ylabAlignment: Alignment = Center,
      topPadding: RelFontSize = 0.2 fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0.2 fts,
      xSecondaryPlotAreaYAxisSettings: Option[AxisSettings] = None,
      xSecondaryData: Seq[(DataSource, List[DataRenderer])] = Nil,
      xSecondaryYLim: Option[(Double, Double)] = None,
      xNoTickLabel: Boolean = false,
      yNoTickLabel: Boolean = false,
      selectionMarker: Option[(Point, Point)] = None,
      yAxisLabelRotation: Double,
      yAxisLabelWidth: Option[Double]
  ) = {
    val id = areaId.getOrElse(new Object)
    Build(
      xyplotarea(
        id,
        externalDataSourceIdx,
        data,
        xAxisSetting,
        yAxisSetting,
        origin,
        xlim,
        ylim,
        xAxisMargin,
        yAxisMargin,
        xgrid,
        ygrid,
        frame,
        xCustomGrid,
        yCustomGrid,
        main,
        mainFontSize,
        mainDistance,
        xlab,
        xlabFontSize,
        xlabDistance,
        xlabAlignment,
        ylab,
        ylabFontSize,
        ylabDistance,
        ylabAlignment,
        topPadding,
        bottomPadding,
        leftPadding,
        rightPadding,
        xSecondaryPlotAreaYAxisSettings,
        xSecondaryData,
        xSecondaryYLim,
        xNoTickLabel,
        yNoTickLabel,
        selectionMarker,
        yAxisLabelRotation,
        yAxisLabelWidth
      )
    ) {
      case (Some(old), BuildEvent,_) =>
        import old._
        xyplotarea(
          id,
          externalDataSourceIdx,
          data,
          xAxisSetting,
          yAxisSetting,
          origin,
          xlim,
          ylim,
          xAxisMargin,
          yAxisMargin,
          xgrid,
          ygrid,
          frame,
          xCustomGrid,
          yCustomGrid,
          main,
          mainFontSize,
          mainDistance,
          xlab,
          xlabFontSize,
          xlabDistance,
          xlabAlignment,
          ylab,
          ylabFontSize,
          ylabDistance,
          ylabAlignment,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xSecondaryPlotAreaYAxisSettings,
          xSecondaryData,
          xSecondaryYLim,
          xNoTickLabel,
          yNoTickLabel,
          selectionMarker,
          yAxisLabelRotation,
          yAxisLabelWidth
        )
      case (Some(old), Scroll(v1, p, plotAreaDescriptor),replay)
          if plotAreaDescriptor.id == id =>
        import old._
        val v = if (v1 > 0) 1.05 else if (v1 < 0) 0.95 else 1.0
        val mappedPoint = mapPoint(
          p,
          plotAreaDescriptor.bounds.get,
          Bounds(xMin, yMin, xMax - xMin, yMax - yMin),
          true
        )

        val xMid = mappedPoint.x
        val yMid = mappedPoint.y
        val xF = (xMid - xMin) / (xMax - xMin)
        val yF = (yMid - yMin) / (yMax - yMin)
        val xMin1 = xMid - (xMax - xMin) * xF * v
        val xMax1 = xMid + (xMax - xMin) * (1 - xF) * v
        val yMin1 = yMid - (yMax - yMin) * yF * v
        val yMax1 = yMid + (yMax - yMin) * (1 - yF) * v
        if (replay) XYPlotArea(None,xMin1,xMax1,yMin1,yMax1)
          else 
        xyplotarea(
          id,
          externalDataSourceIdx,
          data,
          xAxisSetting,
          yAxisSetting,
          origin,
          Some(xMin1 -> xMax1),
          Some(yMin1 -> yMax1),
          xAxisMargin,
          yAxisMargin,
          xgrid,
          ygrid,
          frame,
          xCustomGrid,
          yCustomGrid,
          main,
          mainFontSize,
          mainDistance,
          xlab,
          xlabFontSize,
          xlabDistance,
          xlabAlignment,
          ylab,
          ylabFontSize,
          ylabDistance,
          ylabAlignment,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xSecondaryPlotAreaYAxisSettings,
          xSecondaryData,
          xSecondaryYLim,
          xNoTickLabel,
          yNoTickLabel,
          selectionMarker,
          yAxisLabelRotation,
          yAxisLabelWidth
        )

      case (Some(old), Selection(dragStart, dragTo, plotAreaDescriptor),_)
          if plotAreaDescriptor.id == id =>
        import old._
        val dragStartWorld =
          mapPoint(
            dragStart,
            plotAreaDescriptor.bounds.get,
            Bounds(xMin, yMin, xMax - xMin, yMax - yMin),
            true
          )
        val dragToWorld = mapPoint(
          dragTo,
          plotAreaDescriptor.bounds.get,
          Bounds(xMin, yMin, xMax - xMin, yMax - yMin),
          true
        )

        xyplotarea(
          id,
          externalDataSourceIdx,
          data,
          xAxisSetting,
          yAxisSetting,
          origin,
          Some(xMin -> xMax),
          Some(yMin -> yMax),
          xAxisMargin,
          yAxisMargin,
          xgrid,
          ygrid,
          frame,
          xCustomGrid,
          yCustomGrid,
          main,
          mainFontSize,
          mainDistance,
          xlab,
          xlabFontSize,
          xlabDistance,
          xlabAlignment,
          ylab,
          ylabFontSize,
          ylabDistance,
          ylabAlignment,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xSecondaryPlotAreaYAxisSettings,
          xSecondaryData,
          xSecondaryYLim,
          xNoTickLabel,
          yNoTickLabel,
          Some((dragStartWorld, dragToWorld)),
          yAxisLabelRotation,
          yAxisLabelWidth
        )
      case (Some(old), Drag(dragStart, dragTo, plotAreaDescriptor),replay)
          if plotAreaDescriptor.id == id =>
        import old._
        val dragStartWorld =
          mapPoint(
            dragStart,
            plotAreaDescriptor.bounds.get,
            Bounds(xMin, yMin, xMax - xMin, yMax - yMin),
            true
          )
        val dragToWorld = mapPoint(
          dragTo,
          plotAreaDescriptor.bounds.get,
          Bounds(xMin, yMin, xMax - xMin, yMax - yMin),
          true
        )
        val dragDirection = Point(
          dragStartWorld.x - dragToWorld.x,
          dragStartWorld.y - dragToWorld.y
        )

        val xT = dragDirection.x
        val yT = dragDirection.y

        val xMin1 = xMin + xT
        val xMax1 = xMax + xT
        val yMin1 = yMin + yT
        val yMax1 = yMax + yT
        if (replay) XYPlotArea(None,xMin1,xMax1,yMin1,yMax1)
          else 
        xyplotarea(
          id,
          externalDataSourceIdx,
          data,
          xAxisSetting,
          yAxisSetting,
          origin,
          Some(xMin1 -> xMax1),
          Some(yMin1 -> yMax1),
          xAxisMargin,
          yAxisMargin,
          xgrid,
          ygrid,
          frame,
          xCustomGrid,
          yCustomGrid,
          main,
          mainFontSize,
          mainDistance,
          xlab,
          xlabFontSize,
          xlabDistance,
          xlabAlignment,
          ylab,
          ylabFontSize,
          ylabDistance,
          ylabAlignment,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xSecondaryPlotAreaYAxisSettings,
          xSecondaryData,
          xSecondaryYLim,
          xNoTickLabel,
          yNoTickLabel,
          selectionMarker,
          yAxisLabelRotation,
          yAxisLabelWidth
        )
    }
  }

  def xyplotarea[F: FC](
      id: AnyRef,
      externalDataSourceIdx: Int,
      data: Seq[(DataSource, List[DataRenderer])],
      xAxisSetting: AxisSettings,
      yAxisSetting: AxisSettings,
      origin: Option[Point] = None,
      xlim: Option[(Double, Double)] = None,
      ylim: Option[(Double, Double)] = None,
      xAxisMargin: Double = 0.05,
      yAxisMargin: Double = 0.05,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      frame: Boolean = true,
      xCustomGrid: Boolean = false,
      yCustomGrid: Boolean = false,
      main: String = "",
      mainFontSize: RelFontSize = 1.2 fts,
      mainDistance: RelFontSize = 1.2 fts,
      xlab: String = "",
      xlabFontSize: RelFontSize = 1.0 fts,
      xlabDistance: RelFontSize = 0.5 fts,
      xlabAlignment: Alignment = Center,
      ylab: String = "",
      ylabFontSize: RelFontSize = 1.0 fts,
      ylabDistance: RelFontSize = 0.5 fts,
      ylabAlignment: Alignment = Center,
      topPadding: RelFontSize = 0.2 fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0.2 fts,
      xSecondaryPlotAreaYAxisSettings: Option[AxisSettings] = None,
      xSecondaryData: Seq[(DataSource, List[DataRenderer])] = Nil,
      xSecondaryYLim: Option[(Double, Double)] = None,
      xNoTickLabel: Boolean = false,
      yNoTickLabel: Boolean = false,
      selectionMarker: Option[(Point, Point)] = None,
      yAxisLabelRotation: Double,
      yAxisLabelWidth: Option[Double]
  ) = {

    val xMinMax = data.flatMap {
      case (data, renderers) =>
        renderers.flatMap { renderer =>
          renderer.xMinMax(data)
        }
    }

    val yMinMax = data.flatMap {
      case (data, renderers) =>
        renderers.flatMap { renderer =>
          renderer.yMinMax(data)
        }
    }

    val xLimMin = xlim.map(_._1).filterNot(_.isNaN)
    val xLimMax = xlim.map(_._2).filterNot(_.isNaN)

    val yLimMin = ylim.map(_._1).filterNot(_.isNaN)
    val yLimMax = ylim.map(_._2).filterNot(_.isNaN)

    val dataXMin =
      if (xLimMin.isDefined) 0.0
      else if (xMinMax.isEmpty) 0d
      else xMinMax.map(_.min).min
    val dataXMax =
      if (xLimMax.isDefined) 0.0
      else if (xMinMax.isEmpty) 1d
      else xMinMax.map(_.max).max
    val dataYMin =
      if (yLimMin.isDefined) 0.0
      else if (yMinMax.isEmpty) 0d
      else yMinMax.map(_.min).min
    val dataYMax =
      if (yLimMax.isDefined) 0.0
      else if (yMinMax.isEmpty) 1d
      else yMinMax.map(_.max).max

    val xMin = xAxisSetting.axisFactory match {
      case LinearAxisFactory =>
        math.min(xLimMin.getOrElse {
          dataXMin - xAxisMargin * (dataXMax - dataXMin)
        }, origin.map(_.x).getOrElse(Double.MaxValue))
      case Log10AxisFactory =>
        math.min(
          xLimMin.getOrElse(math.pow(10d, math.log10(dataXMin).floor)),
          origin.map(_.x).getOrElse(Double.MaxValue)
        )
    }

    val xMax = xAxisSetting.axisFactory match {
      case LinearAxisFactory => {
        val xMax1 = xLimMax.getOrElse {
          dataXMax + xAxisMargin * (dataXMax - dataXMin)
        }
        if (xMax1 == xMin) {
          xMax1 + 1
        } else xMax1
      }
      case Log10AxisFactory =>
        val xMax1 = xLimMax.getOrElse {
          math.pow(10d, math.log10(dataXMax).ceil)
        }
        if (xMax1 == xMin) {
          xMax1 + 1
        } else xMax1
      case Log2AxisFactory =>
        val xMax1 = xLimMax.getOrElse {
          math.pow(2d, (math.log10(dataXMax)/math.log10(2d)).ceil)
        }
        if (xMax1 == xMin) {
          xMax1 + 1
        } else xMax1

    }

    val yMin = yAxisSetting.axisFactory match {
      case LinearAxisFactory =>
        math.min(yLimMin.getOrElse {
          dataYMin - yAxisMargin * (dataYMax - dataYMin)
        }, origin.map(_.y).getOrElse(Double.MaxValue))
      case Log10AxisFactory =>
        val v = math.min(
          yLimMin.getOrElse(math.pow(10d, math.log10(dataYMin).floor)),
          origin.map(_.y).getOrElse(Double.MaxValue)
        )
        if (v < 0) 1e-10 else v
      case Log2AxisFactory =>
        val v = math.min(
          yLimMin.getOrElse(math.pow(2d, (math.log10(dataYMin)/math.log10(2d)).floor)),
          origin.map(_.y).getOrElse(Double.MaxValue)
        )
        if (v < 0) 1e-10 else v
    }

    val yMax = yAxisSetting.axisFactory match {
      case LinearAxisFactory => {
        val yMax1 = yLimMax.getOrElse {
          dataYMax + yAxisMargin * (dataYMax - dataYMin)
        }
        if (yMax1 == yMin) yMax1 + 1
        else yMax1
      }
      case Log10AxisFactory =>
        val yMax1 = yLimMax.getOrElse {
          math.pow(10d, math.log10(dataYMax).ceil)
        }
        if (yMax1 == yMin) {
          yMax1 + 1
        } else yMax1
      case Log2AxisFactory =>
        val yMax1 = yLimMax.getOrElse {
          math.pow(2d, (math.log10(dataYMax)/math.log10(2d)).ceil)
        }
        if (yMax1 == yMin) {
          yMax1 + 1
        } else yMax1
    }

    val xAxis =
      xAxisSetting.axisFactory.make(
        xMin,
        xMax,
        xAxisSetting.width.value,
        true,
        false
      )
    val yAxis =
      yAxisSetting.axisFactory.make(
        yMin,
        yMax,
        yAxisSetting.width.value,
        false,
        true
      )

    val xMinV = xAxis.worldToView(xMin)
    val xMaxV = xAxis.worldToView(xMax)
    val yMinV = yAxis.worldToView(yMin)
    val yMaxV = yAxis.worldToView(yMax)

    val yAxisViewMin = yAxisSetting.lineStartFraction * yAxis.width
    val yAxisViewMax = yAxisViewMin + yAxisSetting.width.value * yAxisSetting.lineLengthFraction

    val xAxisViewMin = xAxisSetting.lineStartFraction * xAxis.width
    val xAxisViewMax = xAxisViewMin + xAxisSetting.width.value * xAxisSetting.lineLengthFraction

    val originWX1 = origin.map { origin =>
      xlim.map {
        case (a, b) =>
          if (origin.x < a) a
          else if (origin.x > b) b
          else origin.x
      } getOrElse origin.x
    } getOrElse xMin

    val originWY1 = origin.map { origin =>
      ylim.map {
        case (a, b) =>
          if (origin.y < a) a
          else if (origin.y > b) b
          else origin.y
      } getOrElse origin.y
    } getOrElse yMin

    val noXTick = if (origin.isEmpty) Nil else List(originWX1)
    val noYTick = if (origin.isEmpty) Nil else List(originWY1)

    val (xMajorTicks, xCustomTicks, xAxisElem) =
      xAxisSetting.renderable(xAxis, xNoTickLabel, noXTick)
    val (yMajorTicks, yCustomTicks, yAxisElem) = yAxisSetting.renderable(
      yAxis,
      yNoTickLabel,
      noYTick
    )

    val originX = xAxis.worldToView(originWX1)
    val originY = yAxis.worldToView(originWY1)

    val axes = group(
      translate(xAxisElem, 0, originY),
      translate(yAxisElem, originX, 0),
      FreeLayout
    )

    val dataelem = sequence(data.toList.zipWithIndex.map {
      case ((ds, drs), idx) =>
        DataElem(
          ds,
          xAxis,
          yAxis,
          drs,
          axes.bounds,
          AffineTransform.identity,
          DataSourceIdx(externalDataSourceIdx, idx)
        )
    })

    val xAxisSecondaryPlotArea = ElemOption(if (xSecondaryData.nonEmpty) {
      val axisSettings = xSecondaryPlotAreaYAxisSettings.getOrElse(yAxisSetting)
      val xMinMax = xSecondaryData.flatMap {
        case (data, renderers) =>
          renderers.flatMap { renderer =>
            renderer.xMinMax(data)
          }
      }

      val yMinMax = xSecondaryData.flatMap {
        case (data, renderers) =>
          renderers.flatMap { renderer =>
            renderer.yMinMax(data)
          }
      }
      val dataXMin = xMinMax.map(_.min).min
      val dataXMax = xMinMax.map(_.max).max
      val dataYMin = yMinMax.map(_.min).min
      val dataYMax = yMinMax.map(_.max).max

      // configure ylim

      val yMin = yAxisSetting.axisFactory match {
        case LinearAxisFactory =>
          xSecondaryYLim.map(_._1).getOrElse(dataYMin)
        case Log10AxisFactory =>
          xSecondaryYLim
            .map(_._1)
            .getOrElse(math.pow(10d, math.log10(dataYMin).floor))
      }

      val yMax = yAxisSetting.axisFactory match {
        case LinearAxisFactory => {
          val yMax1 =
            xSecondaryYLim.map(_._2).getOrElse(dataYMax)
          if (yMax1 == yMin) yMax1 + 1
          else yMax1
        }
        case Log10AxisFactory =>
          val yMax1 =
            xSecondaryYLim
              .map(_._2)
              .getOrElse(math.pow(10d, math.log10(dataYMax).ceil))
          if (yMax1 == yMin) {
            yMax1 + 1
          } else yMax1
      }

      val yAxis =
        axisSettings.axisFactory.make(
          yMin,
          yMax,
          axisSettings.width.value,
          horizontal = false,
          inverted = false
        )

      val originY = yAxis.worldToView(originWY1)

      val (yMajorTicks, yCustomTicks, yAxisElem) = axisSettings.renderable(
        yAxis,
        false,
        noYTick
      )

      val axes = group(
        translate(xAxisElem, 0, originY),
        translate(yAxisElem, originX, 0),
        FreeLayout
      )
      val dataElem = sequence(xSecondaryData.toList.zipWithIndex.map {
        case ((ds, drs), idx) =>
          DataElem(
            ds,
            xAxis,
            yAxis,
            drs,
            axes.bounds,
            AffineTransform.identity,
            DataSourceIdx(externalDataSourceIdx, idx)
          )
      })
      val maybeAxisElemX = ElemOption(
        if (axisSettings.visible) Some(yAxisElem) else None
      )
      Some(
        group(group(dataElem, maybeAxisElemX, FreeLayout), axes.m1, FreeLayout)
      )
    } else None)

    val xgridPoints =
      if (xgrid) {
        if (xCustomGrid) (xMajorTicks ++ xCustomTicks).distinct else xMajorTicks
      } else Nil

    val ygridPoints =
      if (ygrid) {
        if (yCustomGrid) (yMajorTicks ++ yCustomTicks).distinct else yMajorTicks
      } else Nil
    val xgridElem = sequence(xgridPoints map { w =>
      val v = xAxis.worldToView(w)
      ShapeElem(
        Shape.line(Point(v, yAxisViewMin), Point(v, yAxisViewMax)),
        stroke =
          if (xgrid) Some(Stroke(lineWidth, dashArray = List(2d))) else None,
        strokeColor = Color.gray5
      )
    })

    val frameStroke = if (frame) Some(Stroke(lineWidth)) else None
    val frameElem =
      ShapeElem(
        Shape.rectangle(
          xMinV,
          yMaxV,
          xMaxV - xMinV,
          math.abs(yMinV - yMaxV),
          anchor = Some(Point(xMinV, yMaxV))
        ),
        stroke = frameStroke,
        fill = Color.transparent
      ).withDescriptor(PlotAreaDescriptor(id, xAxis, yAxis, None,true))

    val ygridElem = sequence(ygridPoints map { w =>
      val v = yAxis.worldToView(w)
      ShapeElem(
        Shape.line(
          Point(xAxisViewMin, v),
          Point(xAxisViewMax, v)
        ),
        stroke =
          if (ygrid) Some(Stroke(lineWidth, dashArray = List(2d))) else None,
        strokeColor = Color.gray5
      )
    })

    val selectionRectangle = ElemOption(selectionMarker.map {
      case (fromW, toW) =>
        val x1 = xAxis.worldToView(fromW.x)
        val x2 = xAxis.worldToView(toW.x)
        val y1 = yAxis.worldToView(fromW.y)
        val y2 = yAxis.worldToView(toW.y)
        val x = math.min(x1, x2)
        val y = math.min(y1, y2)
        val w = math.abs(x1 - x2)
        val h = math.abs(y1 - y2)
        ShapeElem(
          shape = Shape.rectangle(x, y, w, h),
          fill = Color.transparent,
          strokeColor = Color.gray2,
          stroke = Some(Stroke(lineWidth * 0.5, dashArray = List(4d)))
        )
    })

    val renderedPlot =
      group(
        xgridElem,
        ygridElem,
        dataelem,
        axes,
        frameElem,
        selectionRectangle,
        FreeLayout
      )

    val mainBox =
      TextBox(main, fontSize = mainFontSize, width = Some(frameElem.bounds.w), bold=true)
    val xlabBox =
      TextBox(xlab, fontSize = xlabFontSize, width = Some(frameElem.bounds.w))
    val ylabBox =
      TextBox(
        ylab,
        fontSize = ylabFontSize,
        width = Some(frameElem.bounds.h * yAxisLabelWidth.getOrElse(1d))
      )

    val withHorizontalLabels = zgroup(
      (
        zgroup(
          (renderedPlot, 1),
          (
            AlignTo.verticalGapBeforeReference(
              AlignTo.horizontalCenter(mainBox, frameElem.bounds),
              frameElem.bounds,
              mainDistance.value
            ),
            0
          ),
          FreeLayout
        ),
        0
      ),
      (AlignTo.horizontal(xlabBox, frameElem.bounds, xlabAlignment), 1),
      VerticalStack(NoAlignment, xlabDistance)
    )

    val movedFrame = withHorizontalLabels.m1.m1.m5

    val plotWithAxisLabels =
      zgroup(
        (withHorizontalLabels, 1),
        (
          AlignTo.vertical(
            rotate(rotate(ylabBox, 0.5 * math.Pi), yAxisLabelRotation),
            movedFrame.bounds,
            ylabAlignment
          ),
          0
        ),
        HorizontalStack(NoAlignment, ylabDistance)
      )

    val movedSecondary =
      AlignTo.horizontal(
        xAxisSecondaryPlotArea,
        plotWithAxisLabels.m1.m1.m1.m4.m1.bounds,
        Right
      )

    val plotWithXSecondaryPlotArea =
      group(
        plotWithAxisLabels,
        movedSecondary.map(_.m1),
        VerticalStack(NoAlignment)
      )

    val padTop = ShapeElem(
      shape = Shape.circle(topPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padBottom = ShapeElem(
      shape = Shape.circle(bottomPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padLeft = ShapeElem(
      shape = Shape.circle(leftPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padRight = ShapeElem(
      shape = Shape.circle(rightPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val elem = group(
      padTop,
      group(
        padLeft,
        plotWithXSecondaryPlotArea,
        padRight,
        HorizontalStack(Center, 0d fts)
      ),
      padBottom,
      VerticalStack(Center, 0d fts)
    )

    XYPlotArea(Some(elem), xMin, xMax, yMin, yMax)

  }

  sealed trait LegendElem
  case class PointLegend(shape: Shape, color: Color) extends LegendElem
  case class LineLegend(stroke: Stroke, color: Color) extends LegendElem

  type Legend = ElemList[Elems2[ElemList[ShapeElem], TextBox]]

  def legend[F: FC](
      entries: List[(String, List[LegendElem])],
      fontSize: RelFontSize = 1.0 fts,
      width: RelFontSize = 30 fts,
      layout: Layout
  ): Legend = {
    sequence(
      entries.map {
        case (text, elems) =>
          val bbS = fontSize.value * 0.8
          val bb = Bounds(-0.5 * bbS, -0.5 * bbS, bbS, bbS)
          val unitSquare = AlignTo.center(
            fitToBounds(
              ShapeElem(Shape.circle(1d), fill = Color.transparent),
              bb
            ),
            bb
          )
          val elem1 = elems.map { elem =>
            elem match {
              case PointLegend(s, c) =>
                AlignTo.center(
                  fitToBounds(
                    ShapeElem(s, fill = c),
                    bb
                  ),
                  bb
                )
              case LineLegend(s, c) =>
                AlignTo.center(
                  ShapeElem(
                    Shape.line(Point(0, 0), Point(1.5 * bbS, 0)),
                    strokeColor = c,
                    stroke = Some(s.copy(width = s.width * 2))
                  ),
                  bb
                )
            }
          }
          val elemGroup = sequence(unitSquare +: elem1, FreeLayout)
          val textbox =
            TextBox(text, fontSize = fontSize, width = Some(width.value))
          val singleLineHeight = TextBox(
            "A",
            fontSize = fontSize,
            width = Some(width.value)
          ).bounds.h * 0.8
          val resizedElemGroup = fitToHeight(elemGroup, singleLineHeight)
          group(
            resizedElemGroup,
            textbox,
            HorizontalStack(Center, fontSize)
          )
      },
      layout
    )
  }

  type HeatmapLegend = Elems2[Elems2[ElemList[ShapeElem], AxisElem], TextBox]

  def heatmapLegend[F: FC](
      min: Double,
      max: Double,
      color: Colormap = HeatMapColors(0d, 1d),
      fontSize: RelFontSize = 1.0 fts,
      width: RelFontSize = 10 fts,
      height: RelFontSize = 1 fts,
      labelWidth: RelFontSize = 10 fts,
      labelText: String = "",
      numTicks: Int = 2,
      customTicks: Seq[(Double, String)] = Nil,
      horizontal: Boolean = false,
      labelRotation: Double = -0.5 * math.Pi,
      forceMajorTickOnMax: Boolean = false,
      forceMajorTickOnMin: Boolean = false
  ): HeatmapLegend = {

    val color1 = color.withRange(min, max)

    val axisSettings = AxisSettings(
      LinearAxisFactory,
      fontSize = fontSize,
      width = width,
      numTicks = numTicks,
      tickAlignment = 1,
      customTicks = customTicks.map(v => (v._1,AttributedString(v._2))),
      forceMajorTickOnMax = forceMajorTickOnMax,
      forceMajorTickOnMin = forceMajorTickOnMin
    )
    val axis = axisSettings.axisFactory.make(
      min,
      max,
      width.value,
      horizontal,
      !horizontal
    )

    val (_, _, axisElem) =
      axisSettings.renderable(
        axis,
        false
      )

    val axisLabel = {
      val box =
        TextBox(labelText, fontSize = fontSize, width = Some(labelWidth.value))
      rotate(box, labelRotation)
    }

    val n = 200
    val space = (max - min) / n.toDouble
    val colorscale = sequence(
      ((0 until n toList) map { i =>
        val world = axis.min + i * space
        val view = axis.worldToView(world)
        val viewNext =
          if (i == n - 1) axis.worldToView(world + space)
          else axis.worldToView(world + space * 2)

        val x = math.min(view, viewNext)
        val w = math.abs(view - viewNext)

        ShapeElem(
          if (horizontal)
            Shape.rectangle(x, -1d - height.value, w, height.value)
          else Shape.rectangle(1d, x, height.value, w),
          stroke = None,
          fill = color1(world)
        )
      })
    )

    group(
      group(colorscale, axisElem, FreeLayout),
      axisLabel,
      if (horizontal) VerticalStack(Center, lineWidth)
      else HorizontalStack(Center, lineWidth)
    )

  }

}
