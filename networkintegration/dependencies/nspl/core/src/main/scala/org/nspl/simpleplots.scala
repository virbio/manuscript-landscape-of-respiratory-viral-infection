package org.nspl

import data._
import org.nspl.defaultTickFormatter
import org.w3c.dom.Attr

sealed trait LegendConfig
case object NotInLegend extends LegendConfig
case class InLegend(text: String, fused: Boolean = true) extends LegendConfig

case class AttributedString(
    text: String,
    bold: Boolean = false,
    oblique: Boolean= false,
    subScript: Boolean= false,
    superScript: Boolean= false,
    underline: Boolean = false
)
object AttributedString {
  def apply(text: String): AttributedString =
    AttributedString(
      text = text,
      bold = false,
      oblique = false,
      subScript = false,
      superScript = false,
      underline = false
    )
  implicit def conv(s: String): AttributedString =
    AttributedString(
      text = s
    )

  implicit class AttributedStringSyntax(s: String) {
    def attributed = AttributedString(s)
  }
}

/* Factory methods for common plots. */
trait SimplePlots {

  class CommonParameters(
      val areaId: Option[String],
      val xlog: Boolean,
      val ylog: Boolean,
      val ylog2: Boolean,
      val main: String,
      val xlab: String,
      val ylab: String,
      val xnames: Seq[(Double, AttributedString)],
      val ynames: Seq[(Double, AttributedString)],
      val xlim: Option[(Double, Double)],
      val ylim: Option[(Double, Double)],
      val draw1Line: Boolean,
      val extraLegend: Seq[(String, LegendElem)],
      val xLabFontSize: RelFontSize,
      val yLabFontSize: RelFontSize,
      val mainFontSize: RelFontSize,
      val xNumTicks: Int,
      val yNumTicks: Int,
      val xAxisMargin: Double,
      val yAxisMargin: Double,
      val legendFontSize: RelFontSize,
      val legendTextWidth: RelFontSize,
      val xgrid: Boolean,
      val ygrid: Boolean,
      val xWidth: RelFontSize,
      val yHeight: RelFontSize,
      val frame: Boolean,
      val xLabelRotation: Double,
      val yLabelRotation: Double,
      val origin: Option[(Double, Double)],
      val xCustomGrid: Boolean,
      val yCustomGrid: Boolean,
      val legendLayout: Layout,
      val legendToPlotLayout: Layout,
      val xTickLength: RelFontSize,
      val yTickLength: RelFontSize,
      val xLineWidthFraction: Double,
      val yLineWidthFraction: Double,
      val xLineStartFraction: Double,
      val yLineStartFraction: Double,
      val topPadding: RelFontSize,
      val bottomPadding: RelFontSize,
      val leftPadding: RelFontSize,
      val rightPadding: RelFontSize,
      val xLabDistance: RelFontSize,
      val yLabDistance: RelFontSize,
      val mainLabDistance: RelFontSize,
      val xTickSpace: Option[Double],
      val yTickSpace: Option[Double],
      val noLegend: Boolean,
      val xSecondaryYHeight: RelFontSize,
      val xSecondaryYVisible: Boolean,
      val xSecondaryYLim: Option[(Double, Double)],
      val xNoTickLabel: Boolean,
      val yNoTickLabel: Boolean,
      val xTickLabelColors: Double => Option[Color],
      val yTickLabelColors: Double => Option[Color],
      val ablines: List[(Double, Double, Stroke, Color)],
      val xDropOverhangingTickLabels: Boolean,
      val yDropOverhangingTickLabels: Boolean,
      val externalDataSourceIdx: Option[Int],
      val yAxisLabelRotation: Double,
      val yAxisLabelWidth: Option[Double],
      val xTickFormatter: Option[Seq[Double] => Seq[String]],
      val yTickFormatter: Option[Seq[Double] => Seq[String]],
      val xTickFontSize: RelFontSize,
      val yTickFontSize: RelFontSize
  ) {
    def copy(
        areaId: Option[String] = areaId,
        xlog: Boolean = xlog,
        ylog: Boolean = ylog,
        ylog2: Boolean = ylog2,
        main: String = main,
        xlab: String = xlab,
        ylab: String = ylab,
        xnames: Seq[(Double, AttributedString)] = xnames,
        ynames: Seq[(Double, AttributedString)] = ynames,
        xlim: Option[(Double, Double)] = xlim,
        ylim: Option[(Double, Double)] = ylim,
        draw1Line: Boolean = draw1Line,
        extraLegend: Seq[(String, LegendElem)] = extraLegend,
        xLabFontSize: RelFontSize = xLabFontSize,
        yLabFontSize: RelFontSize = yLabFontSize,
        mainFontSize: RelFontSize = mainFontSize,
        xNumTicks: Int = xNumTicks,
        yNumTicks: Int = yNumTicks,
        xAxisMargin: Double = xAxisMargin,
        yAxisMargin: Double = yAxisMargin,
        legendFontSize: RelFontSize = legendFontSize,
        legendTextWidth: RelFontSize = legendTextWidth,
        xgrid: Boolean = xgrid,
        ygrid: Boolean = ygrid,
        xWidth: RelFontSize = xWidth,
        yHeight: RelFontSize = yHeight,
        frame: Boolean = frame,
        xLabelRotation: Double = xLabelRotation,
        yLabelRotation: Double = yLabelRotation,
        origin: Option[(Double, Double)] = origin,
        xCustomGrid: Boolean = xCustomGrid,
        yCustomGrid: Boolean = yCustomGrid,
        legendLayout: Layout = legendLayout,
        legendToPlotLayout: Layout = legendToPlotLayout,
        xTickLength: RelFontSize = xTickLength,
        yTickLength: RelFontSize = yTickLength,
        xLineWidthFraction: Double = xLineWidthFraction,
        yLineWidthFraction: Double = yLineWidthFraction,
        xLineStartFraction: Double = xLineStartFraction,
        yLineStartFraction: Double = yLineStartFraction,
        topPadding: RelFontSize = topPadding,
        bottomPadding: RelFontSize = bottomPadding,
        leftPadding: RelFontSize = leftPadding,
        rightPadding: RelFontSize = rightPadding,
        xLabDistance: RelFontSize = xLabDistance,
        yLabDistance: RelFontSize = yLabDistance,
        mainLabDistance: RelFontSize = mainLabDistance,
        xTickSpace: Option[Double] = xTickSpace,
        yTickSpace: Option[Double] = yTickSpace,
        noLegend: Boolean = noLegend,
        xSecondaryYHeight: RelFontSize = xSecondaryYHeight,
        xSecondaryYVisible: Boolean = xSecondaryYVisible,
        xSecondaryYLim: Option[(Double, Double)] = xSecondaryYLim,
        xNoTickLabel: Boolean = xNoTickLabel,
        yNoTickLabel: Boolean = yNoTickLabel,
        xTickLabelColors: Double => Option[Color] = xTickLabelColors,
        yTickLabelColors: Double => Option[Color] = yTickLabelColors,
        ablines: List[(Double, Double, Stroke, Color)] = ablines,
        xDropOverhangingTickLabels: Boolean = xDropOverhangingTickLabels,
        yDropOverhangingTickLabels: Boolean = yDropOverhangingTickLabels,
        externalDataSourceIdx: Option[Int] = externalDataSourceIdx,
        yAxisLabelRotation: Double = yAxisLabelRotation,
        yAxisLabelWidth: Option[Double] = yAxisLabelWidth,
        xTickFormatter: Option[Seq[Double] => Seq[String]] = xTickFormatter,
        yTickFormatter: Option[Seq[Double] => Seq[String]] = yTickFormatter,
        xTickFontSize: RelFontSize = xTickFontSize,
        yTickFontSize: RelFontSize = yTickFontSize
    ) =
      new CommonParameters(
        areaId = areaId,
        xlog = xlog,
        ylog = ylog,
        ylog2 = ylog2,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xnames = xnames,
        ynames = ynames,
        xlim = xlim,
        ylim = ylim,
        draw1Line = draw1Line,
        extraLegend = extraLegend,
        xLabFontSize = xLabFontSize,
        yLabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        xNumTicks = xNumTicks,
        yNumTicks = yNumTicks,
        xAxisMargin = xAxisMargin,
        yAxisMargin = yAxisMargin,
        legendFontSize = legendFontSize,
        legendTextWidth = legendTextWidth,
        xgrid = xgrid,
        ygrid = ygrid,
        xWidth = xWidth,
        yHeight = yHeight,
        frame = frame,
        xLabelRotation = xLabelRotation,
        yLabelRotation = yLabelRotation,
        origin = origin,
        xCustomGrid = xCustomGrid,
        yCustomGrid = yCustomGrid,
        legendLayout = legendLayout,
        legendToPlotLayout = legendToPlotLayout,
        xTickLength = xTickLength,
        yTickLength = yTickLength,
        xLineWidthFraction = xLineWidthFraction,
        yLineWidthFraction = yLineWidthFraction,
        xLineStartFraction = xLineStartFraction,
        yLineStartFraction = yLineStartFraction,
        topPadding = topPadding,
        bottomPadding = bottomPadding,
        leftPadding = leftPadding,
        rightPadding = rightPadding,
        xLabDistance = xLabDistance,
        yLabDistance = yLabDistance,
        mainLabDistance = mainLabDistance,
        xTickSpace = xTickSpace,
        yTickSpace = yTickSpace,
        noLegend = noLegend,
        xSecondaryYHeight = xSecondaryYHeight,
        xSecondaryYVisible = xSecondaryYVisible,
        xSecondaryYLim = xSecondaryYLim,
        xNoTickLabel = xNoTickLabel,
        yNoTickLabel = yNoTickLabel,
        xTickLabelColors = xTickLabelColors,
        yTickLabelColors = yTickLabelColors,
        ablines = ablines,
        xDropOverhangingTickLabels = xDropOverhangingTickLabels,
        yDropOverhangingTickLabels = yDropOverhangingTickLabels,
        externalDataSourceIdx = externalDataSourceIdx,
        yAxisLabelRotation = yAxisLabelRotation,
        yAxisLabelWidth = yAxisLabelWidth,
        xTickFormatter = xTickFormatter,
        yTickFormatter = yTickFormatter,
        xTickFontSize = xTickFontSize,
        yTickFontSize = yTickFontSize
      )
  }

  object par {
    def apply(
      areaId: Option[String] = None,
        xlog: Boolean = false,
        ylog: Boolean = false,
        ylog2: Boolean = false,
        main: String = "",
        xlab: String = "",
        ylab: String = "",
        xnames: Seq[(Double, AttributedString)] = Seq(),
        ynames: Seq[(Double, AttributedString)] = Seq(),
        xlim: Option[(Double, Double)] = None,
        ylim: Option[(Double, Double)] = None,
        draw1Line: Boolean = false,
        extraLegend: Seq[(String, LegendElem)] = Nil,
        xLabFontSize: RelFontSize = 1 fts,
        yLabFontSize: RelFontSize = 1 fts,
        mainFontSize: RelFontSize = 1 fts,
        xNumTicks: Int = 4,
        yNumTicks: Int = 4,
        xAxisMargin: Double = 0.05,
        yAxisMargin: Double = 0.05,
        legendFontSize: RelFontSize = 1 fts,
        legendTextWidth: RelFontSize = 30 fts,
        xgrid: Boolean = true,
        ygrid: Boolean = true,
        xWidth: RelFontSize = 20 fts,
        yHeight: RelFontSize = 20 fts,
        frame: Boolean = true,
        xLabelRotation: Double = 0d,
        yLabelRotation: Double = 0d,
        origin: Option[(Double, Double)] = None,
        xCustomGrid: Boolean = false,
        yCustomGrid: Boolean = false,
        legendLayout: Layout = ColumnLayout(
          numRows = 10,
          horizontalGap = 0.75 fts,
          verticalGap = 0.4 fts
        ),
        legendToPlotLayout: Layout = HorizontalStack(Anchor, 0.5 fts),
        xTickLength: RelFontSize = 0.4 fts,
        yTickLength: RelFontSize = 0.4 fts,
        xLineWidthFraction: Double = 1d,
        yLineWidthFraction: Double = 1d,
        xLineStartFraction: Double = 0d,
        yLineStartFraction: Double = 0d,
        topPadding: RelFontSize = lineWidth,
        bottomPadding: RelFontSize = 0d fts,
        leftPadding: RelFontSize = 0d fts,
        rightPadding: RelFontSize = lineWidth,
        xLabDistance: RelFontSize = 0.5 fts,
        yLabDistance: RelFontSize = 0.5 fts,
        mainLabDistance: RelFontSize = 0.75 fts,
        xTickSpace: Option[Double] = None,
        yTickSpace: Option[Double] = None,
        noLegend: Boolean = false,
        xSecondaryYHeight: RelFontSize = 5 fts,
        xSecondaryYVisible: Boolean = true,
        xSecondaryYLim: Option[(Double, Double)] = None,
        xNoTickLabel: Boolean = false,
        yNoTickLabel: Boolean = false,
        xTickLabelColors: Double => Option[Color] = _ => None,
        yTickLabelColors: Double => Option[Color] = _ => None,
        ablines: List[(Double, Double, Stroke, Color)] = Nil,
        xDropOverhangingTickLabels: Boolean = false,
        yDropOverhangingTickLabels: Boolean = false,
        externalDataSourceIdx: Option[Int] = None,
        yAxisLabelRotation: Double = 0d,
        yAxisLabelWidth: Option[Double] = None,
        xTickFormatter: Option[Seq[Double] => Seq[String]] = None,
        yTickFormatter: Option[Seq[Double] => Seq[String]] = None,
        xTickFontSize: RelFontSize = 0.8 fts,
        yTickFontSize: RelFontSize = 0.8 fts
    ) =
      new CommonParameters(
        areaId = areaId,
        xlog = xlog,
        ylog = ylog,
        ylog2 = ylog2,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xnames = xnames,
        ynames = ynames,
        xlim = xlim,
        ylim = ylim,
        draw1Line = draw1Line,
        extraLegend = extraLegend,
        xLabFontSize = xLabFontSize,
        yLabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        xNumTicks = xNumTicks,
        yNumTicks = yNumTicks,
        xAxisMargin = xAxisMargin,
        yAxisMargin = yAxisMargin,
        legendFontSize = legendFontSize,
        legendTextWidth = legendTextWidth,
        xgrid = xgrid,
        ygrid = ygrid,
        xWidth = xWidth,
        yHeight = yHeight,
        frame = frame,
        xLabelRotation = xLabelRotation,
        yLabelRotation = yLabelRotation,
        origin = origin,
        xCustomGrid = xCustomGrid,
        yCustomGrid = yCustomGrid,
        legendLayout = legendLayout,
        legendToPlotLayout = legendToPlotLayout,
        xTickLength = xTickLength,
        yTickLength = yTickLength,
        xLineWidthFraction = xLineWidthFraction,
        yLineWidthFraction = yLineWidthFraction,
        xLineStartFraction = xLineStartFraction,
        yLineStartFraction = yLineStartFraction,
        topPadding = topPadding,
        bottomPadding = bottomPadding,
        leftPadding = leftPadding,
        rightPadding = rightPadding,
        xLabDistance = xLabDistance,
        yLabDistance = yLabDistance,
        mainLabDistance = mainLabDistance,
        xTickSpace = xTickSpace,
        yTickSpace = yTickSpace,
        noLegend = noLegend,
        xSecondaryYHeight = xSecondaryYHeight,
        xSecondaryYVisible = xSecondaryYVisible,
        xSecondaryYLim = xSecondaryYLim,
        xNoTickLabel = xNoTickLabel,
        yNoTickLabel = yNoTickLabel,
        xTickLabelColors = xTickLabelColors,
        yTickLabelColors = yTickLabelColors,
        ablines = ablines,
        xDropOverhangingTickLabels = xDropOverhangingTickLabels,
        yDropOverhangingTickLabels = yDropOverhangingTickLabels,
        externalDataSourceIdx = externalDataSourceIdx,
        yAxisLabelRotation = yAxisLabelRotation,
        yAxisLabelWidth = yAxisLabelWidth,
        xTickFormatter = xTickFormatter,
        yTickFormatter = yTickFormatter,
        xTickFontSize = xTickFontSize,
        yTickFontSize = yTickFontSize
      )
  }

  def xyplot[F: FC](data: (DataSource, List[DataRenderer], LegendConfig)*)(
      pars: CommonParameters = par(),
      xSecondaryData: Seq[(DataSource, List[DataRenderer])] = Nil
  ) = {
    import pars._
    val xFac = if (xlog) Log10AxisFactory else LinearAxisFactory
    val yFac =
      if (ylog) Log10AxisFactory
      else if (ylog2) Log2AxisFactory
      else LinearAxisFactory

    val originX = if (xlog) 1.0 else 0.0
    val originY = if (ylog || ylog2) 1.0 else 0.0

    val lines = ablines.map {
      case (a, b, stroke, color) =>
        (
          dataSourceFromRows(List(a -> b)),
          List(polynom(renderer = () => line(stroke = stroke, color = color)))
        )
    } ++ (if (draw1Line)
            List((dataSourceFromRows(List(0.0 -> 1.0)), List(polynom())))
          else Nil)

    val data1 =
      lines ++ data.map(
        x => x._1 -> x._2
      )

    val legend1 =
      if (pars.noLegend) None
      else
        Some(
          legend(
            entries = (
              (data flatMap {
                case (ds, render, conf) =>
                  conf match {
                    case NotInLegend => Nil
                    case InLegend(name1, false) =>
                      val t: List[(String, List[LegendElem])] =
                        render.flatMap(_.asLegend.map {
                          case (name, legend) =>
                            (name1 + name, List(legend))
                        })
                      t
                    case InLegend(name, true) =>
                      val t: List[(String, List[LegendElem])] =
                        List((name, render.flatMap(_.asLegend.map {
                          case (_, legend) =>
                            legend
                        })))
                      t
                  }
              })
                ++ extraLegend.map(v => (v._1, List(v._2)))
            ).toList.distinct,
            fontSize = legendFontSize,
            width = legendTextWidth,
            legendLayout
          )
        )

    val plotArea =
      xyplotareaBuild(
        areaId,
        data1,
        pars.externalDataSourceIdx.getOrElse(0),
        AxisSettings(
          xFac,
          customTicks = xnames,
          fontSize = xTickFontSize,
          numTicks = xNumTicks,
          width = xWidth,
          labelRotation = xLabelRotation,
          tickLength = xTickLength,
          lineLengthFraction = xLineWidthFraction,
          lineStartFraction = xLineStartFraction,
          tickSpace = xTickSpace,
          tickLabelColors = xTickLabelColors,
          dropLabelIfOutside = xDropOverhangingTickLabels,
          tickFormatter = xTickFormatter.getOrElse(defaultTickFormatter)
        ),
        AxisSettings(
          yFac,
          customTicks = ynames,
          fontSize = yTickFontSize,
          numTicks = yNumTicks,
          width = yHeight,
          labelRotation = yLabelRotation,
          tickLength = yTickLength,
          lineLengthFraction = yLineWidthFraction,
          lineStartFraction = yLineStartFraction,
          tickSpace = yTickSpace,
          tickLabelColors = yTickLabelColors,
          dropLabelIfOutside = yDropOverhangingTickLabels,
          tickFormatter = yTickFormatter.getOrElse(defaultTickFormatter)
        ),
        origin = origin.map(x => Point(x._1, x._2)),
        xlim = xlim,
        ylim = ylim,
        xAxisMargin = xAxisMargin,
        yAxisMargin = yAxisMargin,
        xgrid = xgrid,
        ygrid = ygrid,
        frame = frame,
        xCustomGrid = xCustomGrid,
        yCustomGrid = yCustomGrid,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xLabFontSize,
        ylabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        topPadding = topPadding,
        bottomPadding = bottomPadding,
        leftPadding = leftPadding,
        rightPadding = rightPadding,
        xlabDistance = xLabDistance,
        ylabDistance = yLabDistance,
        mainDistance = mainLabDistance,
        xSecondaryPlotAreaYAxisSettings = Some(
          AxisSettings(
            LinearAxisFactory,
            width = xSecondaryYHeight,
            visible = xSecondaryYVisible
          )
        ),
        xSecondaryData = xSecondaryData,
        xSecondaryYLim = xSecondaryYLim,
        xNoTickLabel = xNoTickLabel,
        yNoTickLabel = yNoTickLabel,
        yAxisLabelRotation = yAxisLabelRotation,
        yAxisLabelWidth = yAxisLabelWidth
      )

    group(
      plotArea,
      ElemOption(legend1),
      legendToPlotLayout
    )
  }

  def stackedBarPlot[F: FC](
      data: DataSource,
      legend: Seq[(Int, String, Colormap)],
      xCol: Int = 0,
      relative: Boolean = false,
      stroke: Stroke = Stroke(lineWidth),
      strokeColor: Color = Color.black,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 1 fts
  )(pars: CommonParameters) = {
    {
      val data1: Seq[Seq[VectorRow]] = data.iterator
        .map { row =>
          val x = row(xCol)
          val sum = if (relative) legend.map(x => row(x._1)).sum else 1.0

          val data = legend.map(x => row(x._1) / sum)

          val accum: Seq[Double] =
            data.drop(1).scanLeft(data.head)((y, l) => y + l)

          accum zip (0.0 +: accum.dropRight(1)) zip legend.map(_._2) map {
            case ((y1, y2), label) =>
              VectorRow(Vector(x, y1, y2), if (y1 != y2) label else "")
          }

        }
        .toVector
        .transpose

      val legend1 = legend.zipWithIndex.map(x => (x._2, x._1._2, x._1._3))

      val renderers = legend1.zip(data1).map {
        case ((idx, label, color), data) =>
          val ds: DataSource = data
          (
            ds,
            List(
              bar(
                fill = color,
                fillCol = ds.dimension + 1,
                widthCol = ds.dimension + 2,
                yCol = 1,
                yCol2 = Some(2),
                stroke = stroke,
                strokeColor = strokeColor,
                labelText = labelText,
                labelFontSize = labelFontSize
              )
            ),
            InLegend(label)
          )
      }

      xyplot(renderers: _*)(pars)

    }
  }

  case class StackedBarPlotElement(
      value: Double,
      label: String,
      fillColor: Color,
      labelColor: Color
  )

  case class StackedBarPlotColumn(
      heightValues: Vector[StackedBarPlotElement],
      x: Double
  )

  def stackedBarPlot2[F: FC](
      bars: Seq[StackedBarPlotColumn],
      relative: Boolean = false,
      stroke: Stroke = Stroke(lineWidth),
      strokeColor: Color = Color.black,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 1 fts,
      labelFontConfig: Option[FontConfiguration] = None,
      fitLabelToBox: Boolean = false
  )(
      pars: CommonParameters,
      xSecondaryData: Seq[(DataSource, List[DataRenderer])] = Nil
  ) = {
    {
      val data1 = bars.flatMap { elem =>
        val x = elem.x
        val heightValues = elem.heightValues.map(_.value)
        val sum = if (relative) heightValues.sum else 1.0

        val data = heightValues.map(_ / sum)
        val labels = elem.heightValues.map(_.label)
        val fillColors = elem.heightValues.map(_.fillColor)
        val labelColors = elem.heightValues.map(_.labelColor)

        val accum: Seq[Double] =
          data.drop(1).scanLeft(data.head)((y, l) => y + l)

        accum zip (0.0 +: accum.dropRight(1)) zip labels zip fillColors zip labelColors map {
          case ((((y1, y2), label), fillColor), labelColor) =>
            (
              VectorRow(Vector(x, y1, y2), if (y1 != y2) label else ""),
              fillColor,
              labelColor
            )
        }

      }.toVector

      val renderers = data1.map {
        case (row, fillColor, labelColor) =>
          (
            Vector(row),
            List(
              bar(
                fill = fillColor,
                fillCol = row.dimension + 1,
                widthCol = row.dimension + 1,
                yCol = 1,
                yCol2 = Some(2),
                stroke = stroke,
                strokeColor = strokeColor,
                labelText = labelText,
                labelFontSize = labelFontSize,
                labelColor = labelColor,
                labelFontConfig = labelFontConfig,
                fitLabelToBox = fitLabelToBox
              )
            ),
            NotInLegend
          )
      }

      xyplot(renderers: _*)(pars, xSecondaryData)

    }
  }

  def boxplotFromColumns[F: FC](
      data: DataSourceWithQuantiles,
      xnames: Seq[String] = Nil,
      boxColor: Colormap = Color.gray4,
      boxWidth: Double = 1d
  )(par: CommonParameters) = {

    val bxdata = boxplotDataFromColumns(data)

    boxplotImpl(bxdata, Some(data), boxColor, xnames, boxWidth)(par)
  }

  def boxplotImpl[F: FC](
      bxdata: DataSource,
      originalData: Option[DataSourceWithQuantiles],
      boxColor: Colormap = Color.gray4,
      xnames: Seq[String] = Nil,
      boxWidth: Double = 1d,
      xJitterMaxWidth: Double = 0.75,
      xJitterDecay: Double = -2d
  )(par: CommonParameters) = {

    val min = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).min
    val max = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).max

    val xnames1 =
      if (xnames.isEmpty)
        bxdata.iterator.toList
          .map(ds => (ds(0), ds.label))
          .filter(_._2.nonEmpty)
      else xnames.zipWithIndex.map(x => x._2.toDouble -> x._1)

    val columnData = dataSourceFromRows(
      originalData.toList.flatMap { originalData =>
        val boxData = bxdata.iterator.toVector
        0 until originalData.dimension flatMap {
          columnIdx =>
            val boxDataOfColumn = boxData(columnIdx)
            val median = boxDataOfColumn(1)
            val p25 = boxDataOfColumn(2)
            val p75 = boxDataOfColumn(3)

            originalData.iterator.map { row =>
              val noise =
                if (xJitterMaxWidth == 0) 0d
                else {
                  val jitterStrength = xJitterMaxWidth * math.exp(
                    xJitterDecay * math.abs(
                      row(columnIdx) - median
                    ) / (p75 - p25)
                  )

                  ((scala.util.Random.nextDouble - 0.5) / (boxWidth)) * jitterStrength
                }

              VectorRow(columnIdx.toDouble + noise, row.apply(columnIdx))
            }.toList
        }
      }
    )

    xyplotareaBuild(
        par.areaId,
      List(
        bxdata -> List(boxwhisker(fill = boxColor, width = boxWidth)),
        columnData -> List(
          point()
        )
      ),
      par.externalDataSourceIdx.getOrElse(0),
      AxisSettings(
        LinearAxisFactory,
        customTicks = xnames1.map(x => x._1 -> AttributedString.conv(x._2)),
        numTicks = if (xnames1.isEmpty) 5 else 0,
        fontSize = par.xTickFontSize,
        width = par.xWidth,
        labelRotation = par.xLabelRotation,
        tickLength = par.xTickLength,
        lineLengthFraction = par.xLineWidthFraction,
        lineStartFraction = par.xLineStartFraction,
        tickSpace = par.xTickSpace,
        tickLabelColors = par.xTickLabelColors,
        dropLabelIfOutside = par.xDropOverhangingTickLabels,
        tickFormatter = par.xTickFormatter.getOrElse(defaultTickFormatter)
      ),
      AxisSettings(
        LinearAxisFactory,
        fontSize = par.yTickFontSize,
        width = par.yHeight,
        labelRotation = par.yLabelRotation,
        numTicks = par.yNumTicks,
        customTicks = par.ynames,
        tickLength = par.yTickLength,
        lineLengthFraction = par.yLineWidthFraction,
        lineStartFraction = par.yLineStartFraction,
        tickSpace = par.yTickSpace,
        tickLabelColors = par.yTickLabelColors,
        dropLabelIfOutside = par.yDropOverhangingTickLabels,
        tickFormatter = par.yTickFormatter.getOrElse(defaultTickFormatter)
      ),
      None,
      xlim = None,
      ylim = par.ylim.orElse(Some(min -> max)),
      xgrid = par.xgrid,
      ygrid = par.ygrid,
      frame = par.frame,
      main = par.main,
      xlab = par.xlab,
      ylab = par.ylab,
      xlabFontSize = par.xLabFontSize,
      ylabFontSize = par.yLabFontSize,
      mainFontSize = par.mainFontSize,
      xNoTickLabel = par.xNoTickLabel,
      yNoTickLabel = par.yNoTickLabel,
      yAxisLabelRotation = par.yAxisLabelRotation,
      yAxisLabelWidth = par.yAxisLabelWidth
    )
  }

  def densityPlotFromGroupLabels[F: FC](
      groups: Seq[(String, Seq[(String, Double, Double)], Colormap)],
      bandwidths: Map[String, Double] = Map.empty,
      sizes: Map[String, Double] = Map.empty,
      defaultSize: Double = 1d,
      jitterScale: Double = 0.001
  )(par: CommonParameters) = {

    import org.nspl.data._
    val withJitter = groups.filter(_._2.nonEmpty).map {
      case ((name, col, colormap)) =>
        val vs = col.map(_._2).toArray
        val std = math.sqrt(sampleVariance(vs.toSeq))
        val withJitter = col.map {
          case (label, v, colorValue) =>
            val jitter = {
              val densityEstimateAtPoint = org.nspl.data.KDE
                .univariate(
                  vs,
                  v,
                  bandwidths
                    .get(name)
                    .getOrElse(
                      if (std.isNaN || std == 0d) 1d else std / 3
                    )
                )
              assert(densityEstimateAtPoint.isFinite, s"na: ${vs.toList}")

              if (vs.size < 10) 0d
              else
                (scala.util.Random.nextDouble - 0.5) * densityEstimateAtPoint * jitterScale
            }
            (label, (jitter, v, colorValue))
        }

        (
          name,
          withJitter,
          withJitter
            .map(v => math.abs(v._2._1))
            .maxOption
            .filter(_ > 0)
            .getOrElse(jitterScale),
          colormap
        )
    }

    val m = withJitter.map(_._3).max * 2

    xyplot(withJitter.zipWithIndex.map {
      case ((name, zeroCentered, _, colormap), idx) =>
        val shifted = zeroCentered.map {
          case (label, (x, y, colorvalue)) =>
            VectorRow(x + idx * m, y, colorvalue).withLabel(label)
        }
        (
          shifted,
          point(
            color = colormap,
            colorCol = 2,
            size = sizes.get(name).getOrElse(defaultSize)
          )
        )
    }: _*)(
      par.copy(
        xnames = groups
          .map(_._1)
          .zipWithIndex
          .map(v => (v._2.toDouble * m) -> s"${v._1}"),
        xNumTicks = 0,
        xTickLength = 0d fts
      )
    )

  }

  def boxplotFromGroupLabels[T: Ordering, F: FC](
      data: Seq[(T, Double)],
      boxColor: Colormap = Color.gray4,
      useLabels: Boolean = true
  )(par: CommonParameters) = {
    val bxdata = boxplotDataFromGroupLabels(data.toSeq)

    boxplotImpl(
      bxdata,
      None,
      boxColor,
      if (useLabels) bxdata.iterator.map(_.label).toList else Nil
    )(par)
  }

  def binnedboxplot[F: FC](
      dim1: Seq[Double],
      dim2: Seq[Double],
      xnames: Seq[String] = Nil,
      bins: Int = 10,
      boxColor: Colormap = Color.gray4,
      xgrid: Boolean = false
  )(par: CommonParameters) = {

    val newpar = par.copy(
      xgrid = xgrid,
      xlim = Some(dim2.min -> dim2.max),
      ylim = Some(dim1.min -> dim1.max)
    )

    xyplot(
      boxplotDataFrom2D(
        dim1,
        dim2,
        1 to bins map (i => i / bins.toDouble),
        Vector.fill(bins)(0d)
      ) -> boxwhisker(fill = boxColor)
    )(newpar)
  }

  def contourplot[F: FC](
      xlim: (Double, Double),
      ylim: (Double, Double),
      f: (Double, Double, Double, Double) => Double,
      n: Int,
      levels: Int
  )(par: CommonParameters) = {
    val contours = data.contour(
      xlim._1,
      xlim._2,
      ylim._1,
      ylim._2,
      n,
      levels
    )(f)

    val newpar = par.copy(xlim = Some(xlim), ylim = Some(ylim))

    xyplot(
      linesegments(contours)
    )(newpar)
  }

  def rasterplot[F: FC](
      data: DataSource,
      colormap: Colormap = HeatMapColors(0, 1),
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      labelData: Option[DataSource] = None,
      valueText: Boolean = false,
      valueColor: Color = Color.black,
      valueFontSize: RelFontSize = 0.4 fts,
      zlim: Option[(Double, Double)] = None,
      transparentPixels: Option[Double] = None,
      zlab: String = "",
      horizontalMarkers: Seq[(Int, Stroke, Color)] = Nil,
      verticalMarkers: Seq[(Int, Stroke, Color)] = Nil,
      zNumTicks: Int = 3,
      xLabelRotation: Double = -.5 * math.Pi,
      zCustomTicks: Seq[(Double, String)] = Nil
  )(
      par: CommonParameters,
      xSecondaryData: Seq[(DataSource, List[DataRenderer])] = Nil
  ) = {
    val minmaxx = data.columnMinMax(xCol)
    val minmaxy = data.columnMinMax(yCol)
    val minmaxz = data.columnMinMax(zCol)
    val xmin = minmaxx.map(_.min).getOrElse(0d)
    val xmax = minmaxx.map(_.max).getOrElse(1d)
    val ymin = minmaxy.map(_.min).getOrElse(0d)
    val ymax = minmaxy.map(_.max).getOrElse(1d)
    val zmin = zlim.map(_._1).orElse(minmaxz.map(_.min)).getOrElse(0d)
    val zmax = ({
      zlim.map(_._2).orElse(minmaxz.map(_.max)).map { v =>
        if (v == zmin) zmin + 1
        else v
      }
    }).getOrElse(1d)
    import par.{xLabelRotation => _, _}
    val eitherLegend =
      if (noLegend || extraLegend.nonEmpty)
        scala.util.Left(
          legend(
            entries =
              if (noLegend) Nil
              else extraLegend.toList.map(v => (v._1, List(v._2))),
            fontSize = legendFontSize,
            width = legendTextWidth,
            legendLayout
          )
        )
      else
        scala.util.Right(
          heatmapLegend(
            zmin,
            zmax,
            colormap,
            labelText = zlab,
            numTicks = zNumTicks,
            customTicks = zCustomTicks
          )
        )

    group(
      xyplotareaBuild(
        par.areaId,
        List(
          data -> List(
            point(
              pointSizeIsInDataSpaceUnits = true,
              color = colormap.withRange(zmin, zmax),
              shapes = Vector(Shape.rectangle(-0.5, -0.5, 1.0, 1.0)),
              size = 1d,
              valueText = valueText && labelData.isEmpty,
              labelColor = valueColor,
              labelFontSize = valueFontSize,
              transparent = transparentPixels,
              translate = (0.5, 0.5),
              noDescriptor = false
            )
          )
        ) ++ labelData.toList.map { labelData =>
          labelData -> List(
            point(
              pointSizeIsInDataSpaceUnits = true,
              color = Color.transparent,
              shapes = Vector(Shape.rectangle(-0.5, -0.5, 1.0, 1.0)),
              size = 1d,
              valueText = true,
              labelColor = valueColor,
              labelFontSize = valueFontSize,
              transparent = transparentPixels,
              noDescriptor = false
            )
          )
        } ++
          horizontalMarkers.map {
            case (pos, stroke, color) =>
              (
                dataSourceFromRows(
                  Vector((xmin, pos.toDouble, xmax + 1, pos.toDouble))
                ),
                List(lineSegment(strokes = List(stroke), color = color))
              )
          } ++
          verticalMarkers.map {
            case (pos, stroke, color) =>
              (
                dataSourceFromRows(
                  Vector((pos.toDouble, ymin, pos.toDouble, ymax + 1))
                ),
                List(lineSegment(strokes = List(stroke), color = color))
              )
          },
        par.externalDataSourceIdx.getOrElse(0),
        AxisSettings(
          LinearAxisFactory,
          customTicks = xnames,
          fontSize = xTickFontSize,
          numTicks = xNumTicks,
          tickSpace = xTickSpace,
          numMinorTicksFactor = 0,
          tickLength = xTickLength,
          labelRotation = xLabelRotation,
          width = xWidth,
          tickLabelColors = xTickLabelColors,
          dropLabelIfOutside = xDropOverhangingTickLabels,
          tickFormatter = xTickFormatter.getOrElse(defaultTickFormatter)
        ),
        AxisSettings(
          LinearAxisFactory,
          customTicks = ynames,
          fontSize = yTickFontSize,
          numTicks = yNumTicks,
          tickSpace = yTickSpace,
          numMinorTicksFactor = 0,
          tickLength = yTickLength,
          labelRotation = yLabelRotation,
          width = yHeight,
          tickLabelColors = yTickLabelColors,
          dropLabelIfOutside = xDropOverhangingTickLabels,
          tickFormatter = yTickFormatter.getOrElse(defaultTickFormatter)
        ),
        xlim = Some(xmin -> (xmax + 1)),
        ylim = Some(ymin -> (ymax + 1)),
        xAxisMargin = 0.0,
        yAxisMargin = 0d,
        frame = frame,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xLabFontSize,
        ylabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        mainDistance = par.mainLabDistance,
        xSecondaryPlotAreaYAxisSettings = Some(
          AxisSettings(
            LinearAxisFactory,
            width = xSecondaryYHeight,
            visible = xSecondaryYVisible
          )
        ),
        xSecondaryData = xSecondaryData,
        xSecondaryYLim = xSecondaryYLim,
        xNoTickLabel = par.xNoTickLabel,
        yNoTickLabel = par.yNoTickLabel,
        xgrid = xgrid,
        ygrid = ygrid,
        yAxisLabelRotation = par.yAxisLabelRotation,
        yAxisLabelWidth = par.yAxisLabelWidth
      ),
      ElemEither(eitherLegend),
      HorizontalStack(Anchor, 1 fts)
    )

  }

  def densityPlot[F: FC](
      data: Seq[(IndexedSeq[Double], String)],
      lineWidth: RelFontSize = 0.2 fts
  )(
      pars: CommonParameters = par()
  ) =
    xyplot(
      data.zipWithIndex.map {
        case ((d, name), idx) =>
          (
            density(d),
            line(stroke = Stroke(lineWidth), color = colorPick(idx, data.size)),
            InLegend(name)
          )
      }: _*
    )(pars)

  def hist[F: FC](
      data: Seq[(IndexedSeq[Double], String)],
      breaks: Int = 50,
      opacity: Int = 255,
      plotBars: Boolean = true,
      relative: Boolean = false
  )(
      pars: CommonParameters = par()
  ) =
    histFromHistogramData(
      data = data.map {
        case (d, name) =>
          val hd = HistogramData(d, breaks)
          val relativeOrNot = if (relative) hd.relative else hd
          (relativeOrNot, name)
      },
      opacity,
      plotBars
    )(pars)

  def histFromHistogramData[F: FC](
      data: Seq[(HistogramData, String)],
      opacity: Int = 255,
      plotBars: Boolean = true
  )(
      pars: CommonParameters = par()
  ) =
    xyplot(
      data.zipWithIndex.map {
        case ((d, name), idx) =>
          (
            d,
            if (plotBars)
              bar(
                fill = colorPick(idx, data.size).copy(a = opacity),
                stroke = Stroke(0d fts)
              )
            else line(color = colorPick(idx, data.size).copy(a = opacity)),
            InLegend(name)
          )
      }: _*
    )(pars)

  def radarPlot[F: FC](
      data: DataSource,
      metaData: DataSource,
      colorCol: Int,
      lineColors: Colormap,
      strokeCol: Int,
      tickLabelOnOneAxis: Boolean = true,
      gridColor: Color = Color.black,
      gridStroke: Stroke = Stroke(lineWidth)
  )(pars: CommonParameters = par()) = {

    val numAxes = data.dimension
    val axisAngles = 0 until numAxes map (i => i * math.Pi * 2d / numAxes)
    val axisSettings = 0 until numAxes map { i =>
      AxisSettings(
        LinearAxisFactory,
        customTicks = pars.xnames,
        fontSize = pars.xLabFontSize,
        numTicks = pars.xNumTicks,
        width = pars.xWidth,
        labelRotation = pars.xLabelRotation - axisAngles(i),
        tickLength = pars.xTickLength,
        lineLengthFraction = pars.xLineWidthFraction,
        lineStartFraction = pars.xLineStartFraction,
        tickSpace = pars.xTickSpace,
        tickLabelColors = pars.xTickLabelColors,
        dropLabelIfOutside = pars.xDropOverhangingTickLabels
      )
    }
    val axes = 0 until numAxes map { i =>
      val minmax = data.columnMinMax(i).get

      val xLimMin = pars.xlim.map(_._1).filterNot(_.isNaN)
      val xLimMax = pars.xlim.map(_._2).filterNot(_.isNaN)

      val dataXMin =
        if (xLimMin.isDefined) 0.0
        else minmax.min
      val dataXMax =
        if (xLimMax.isDefined) 0.0
        else minmax.max

      val xMin = axisSettings(i).axisFactory match {
        case LinearAxisFactory =>
          xLimMin.getOrElse {
            dataXMin - pars.xAxisMargin * (dataXMax - dataXMin)
          }
        case Log10AxisFactory =>
          xLimMin.getOrElse(math.pow(10d, math.log10(dataXMin).floor))

      }

      val xMax = axisSettings(i).axisFactory match {
        case LinearAxisFactory => {
          val xMax1 = xLimMax.getOrElse {
            dataXMax + pars.xAxisMargin * (dataXMax - dataXMin)
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

      }

      axisSettings(i).axisFactory.make(
        xMin,
        xMax,
        axisSettings(i).width.value,
        true,
        false
      )
    }

    val rows = data.iterator.toList

    val colorMap = lineColors

    val colorValues = metaData.iterator.map(_.apply(colorCol)).toVector
    val lineWidths = metaData.iterator.map(_.apply(strokeCol)).toVector

    val points = rows.zipWithIndex.flatMap {
      case (row, idx) =>
        val views = 0 until numAxes map { i =>
          val axis = axes(i)
          val w = row.apply(i)
          val radius = axis.worldToView(w)
          val vX = radius * math.cos(axisAngles(i))
          val vY = radius * math.sin(axisAngles(i))
          (vX, vY)
        }
        0 until numAxes flatMap { i =>
          val (vX, vY) = views(i)
          val point = ShapeElem(
            Shape.circle(1d),
            fill = colorMap(colorValues(idx))
          ).translate(vX, vY)

          val line = {
            val (prevX, prevY) = views(if (i == 0) numAxes - 1 else i - 1)

            ShapeElem(
              Shape.line(Point(vX, vY), Point(prevX, prevY)),
              strokeColor = colorMap(colorValues(idx)),
              stroke = Some(Stroke(lineWidth * lineWidths(idx)))
            )

          }
          List(point, line)
        }
    }

    val (ticks, axesRenderables) = axes.zipWithIndex.map {
      case (axis, i) =>
        val (majorTicks, customTicks, elem) =
          axisSettings(i).renderable(
            axis,
            if (!tickLabelOnOneAxis) pars.xNoTickLabel
            else if (i == 0) pars.xNoTickLabel
            else true,
            Nil
          )
        (majorTicks ++ customTicks, elem.rotate(axisAngles(i), 0d, 0d))
    }.unzip

    val grids = ticks.transpose.flatMap { gridline =>
      val views = gridline.zipWithIndex.map {
        case (w, i) =>
          val radius = axes(i).worldToView(w)
          val vX = radius * math.cos(axisAngles(i))
          val vY = radius * math.sin(axisAngles(i))
          (vX, vY)
      }
      views.zipWithIndex.map {
        case ((vX, vY), i) =>
          val (prevX, prevY) = views(if (i == 0) numAxes - 1 else i - 1)
          ShapeElem(
            Shape.line(Point(vX, vY), Point(prevX, prevY)),
            strokeColor = gridColor,
            stroke = Some(gridStroke)
          )

      }
    }

    val labels = data.columnNames.zipWithIndex.map {
      case (name, i) =>
        val radius = axes(i).worldToView(axes(i).max) + (1.5 fts).value
        val angle = axisAngles(i)
        val vX = radius * math.cos(angle)
        val vY = radius * math.sin(angle)
        TextBox(name, loc = Point(vX, vY))
    }

    val radar = group(
      sequence(grids),
      sequence(labels),
      sequence(points),
      sequence(axesRenderables),
      FreeLayout
    )

    val mainBox =
      TextBox(
        pars.main,
        fontSize = pars.mainFontSize,
        width = Some(radar.bounds.w)
      )

    val withMainLabel =
      zgroup(
        (radar, 1),
        (
          AlignTo.verticalGapBeforeReference(
            AlignTo.horizontalCenter(mainBox, radar.bounds),
            radar.bounds,
            pars.mainLabDistance.value
          ),
          0
        ),
        FreeLayout
      )
    withMainLabel
  }

  def xyzplot[F: FC](data: (DataSource, List[DataRenderer3D], LegendConfig)*)(
      zNear: Double = 1d,
      zFar: Double = 2000d,
      fieldOfViewAngles: Double = 60,
      cameraPosition: Math3D.Vec3 = Math3D.Vec3(50f, 50f, 300f),
      cameraTarget: Math3D.Vec3 = Math3D.Vec3(0f, 0f, 0f),
      cameraUp: Math3D.Vec3 = Math3D.Vec3(0f, 1f, 0f),
      dragFactor: Double = 0.5,
      scrollFactor: Double = 0.001,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      extraLegend: Seq[(String, LegendElem)] = Nil,
      legendFontSize: RelFontSize = 1 fts,
      legendTextWidth: RelFontSize = 30 fts,
      legendLayout: Layout = ColumnLayout(
        numRows = 10,
        horizontalGap = 0.75 fts,
        verticalGap = 0.4 fts
      ),
      legendToPlotLayout: Layout = HorizontalStack(Anchor, 0.5 fts),
      topPadding: RelFontSize = 0d fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0d fts,
      externalDataSourceIdx: Option[Int] = None,
      areaId : Option[String ] = None
  ) = {

    val aspect = xWidth.value / yHeight.value

    val data1 =
      data.map(x => x._1 -> x._2)

    val legend1 =
      legend(
        entries = (
          (data flatMap {
            case (ds, render, conf) =>
              conf match {
                case NotInLegend => Nil
                case InLegend(name1, false) =>
                  val t: List[(String, List[LegendElem])] =
                    render.flatMap(_.asLegend.map {
                      case (name, legend) =>
                        (name1 + name, List(legend))
                    })
                  t
                case InLegend(name, true) =>
                  val t: List[(String, List[LegendElem])] =
                    List((name, render.flatMap(_.asLegend.map {
                      case (_, legend) =>
                        legend
                    })))
                  t
              }
          })
            ++ extraLegend.map(v => (v._1, List(v._2)))
        ).toList.distinct,
        fontSize = legendFontSize,
        width = legendTextWidth,
        legendLayout
      )

    val plotArea =
      xyzplotareaBuild(
        data1,
        externalDataSourceIdx.getOrElse(0),
        zNear,
        zFar,
        fieldOfViewAngles,
        cameraPosition,
        cameraTarget,
        cameraUp,
        topPadding,
        bottomPadding,
        leftPadding,
        rightPadding,
        xWidth,
        yHeight,
        dragFactor,
        scrollFactor,
        areaId
      )

    group(
      plotArea,
      legend1,
      legendToPlotLayout
    )
  }

}
