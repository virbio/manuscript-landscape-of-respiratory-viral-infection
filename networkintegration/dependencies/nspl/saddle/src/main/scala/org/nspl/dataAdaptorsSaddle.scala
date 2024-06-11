package org.nspl

import org.nspl.data._
import org.saddle._

package object saddle {

  def barplotVertical[F: FC](
      series: Series[String, Double],
      color: Colormap = Color.white,
      yLabFontSize: Option[RelFontSize] = None
  )(par: CommonParameters) =
    xyplot(
      series.toSeq
        .map(_._2)
        .zipWithIndex
        .map(x => x._1 -> x._2.toDouble) -> bar(
        horizontal = true,
        fill = color,
        fillCol = 0
      )
    )(
      par.copy(
        yLabFontSize = yLabFontSize
          .getOrElse(math.min(2.0, par.yHeight.factor / series.length) fts),
        ynames = series.toSeq
          .map(_._1.toString)
          .zipWithIndex
          .map(x => AttributedString(x._1) -> x._2.toDouble)
          .map(_.swap),
        yNumTicks = 0
      )
    )

  def barplotHorizontal[F: FC](
      series: Series[String, Double],
      xLabFontSize: Option[RelFontSize] = None,
      color: Colormap = Color.white
  )(par: CommonParameters) =
    xyplot(
      series.toSeq
        .map(_._2)
        .zipWithIndex
        .map(x => x._1 -> x._2.toDouble)
        .map(_.swap) -> bar(horizontal = false, fill = color, fillCol = 1)
    )(
      par.copy(
        xTickFontSize = xLabFontSize
          .getOrElse((par.xWidth.factor / series.length) fts),
        xnames = series.toSeq
          .map(_._1.toString)
          .zipWithIndex
          .map(x => AttributedString(x._1) -> x._2.toDouble)
          .map(_.swap),
        xNumTicks = 0
      )
    )

  def rasterplotFromFrame[RX, CX, F: FC](
      dataFrame: Frame[RX, CX, Double],
      labelData: Option[Frame[RX,CX,Double]] = None,
      colormap: Colormap = HeatMapColors(0, 1),
      valueText: Boolean = false,
      valueColor: Color = Color.black,
      valueFontSize: RelFontSize = 0.4 fts,
      zlim: Option[(Double, Double)] = None,
      transparentPixels: Option[Double] = None,
      xTickLabelColors: Vector[Color] = Vector.empty,
      yTickLabelColors: Vector[Color] = Vector.empty,
      zlab: String = "",
      zNumTicks: Int = 3,
      xLabelRotation: Double = -.5 * math.Pi
  )(par: CommonParameters) = {
    def get(v: Vector[Color], i: Int) = if (v.length > i) Some(v(i)) else None

        
    rasterplot(
      asRaster(dataFrame.toMat),
      colormap,
      labelData = labelData.map(d => asRaster(d.toMat)),
      valueText = valueText,
      valueColor = valueColor,
      valueFontSize = valueFontSize,
      zlim = zlim,
      zlab = zlab,
      zNumTicks = zNumTicks,
      xLabelRotation = xLabelRotation
    )(
      par.copy(
        xTickFontSize = (par.xWidth.factor / dataFrame.numCols) fts,
        yTickFontSize = (par.yHeight.factor / dataFrame.numRows) fts,
        xnames = dataFrame.colIx.toSeq
          .map(_.toString)
          .zipWithIndex
          .map(x => x._2.toDouble + 0.5 -> x._1),
        xTickLabelColors = world => get(xTickLabelColors, (world - 0.5).toInt),
        ynames = dataFrame.rowIx.toSeq
          .map(_.toString)
          .zipWithIndex
          .map(x => x._2.toDouble + 0.5 -> x._1),
        yTickLabelColors = world => get(yTickLabelColors, (world - 0.5).toInt),
        yNumTicks = 0,
        xNumTicks = 0,
        xTickLength = 0d fts,
        yTickLength = 0d fts
      )
    )

  }

  def asRaster(mat: Mat[Double]): DataMatrix =
    new DataMatrix(mat.contents, mat.numCols, mat.numRows)

  implicit def dataSourceFromMat(mat: Mat[Double]): DataTable =
    new DataTable(mat.contents, mat.numCols)

  implicit def dataSourceFrom1DVec(vec: Vec[Double]): DataSourceWithQuantiles =
    indexed(vec.toSeq)

  implicit def dataSourceFromSeries[R](
      s: Series[R, Double]
  ): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {
      def iterator =
        s.toSeq.iterator.zipWithIndex
          .map(x => VectorRow(Vector(x._2, x._1._2), x._1._1.toString))
      def dimension = 2
      def columnMinMax(i: Int): Option[MinMax] = i match {
        case 0 if s.length > 0 => Some(MinMaxImpl(0, s.length - 1d))
        case 1 if s.length > 0 => Some(MinMaxImpl(s.min.get, s.max.get))
        case _                 => None
      }
      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        assert(i == 1 || i == 0)
        val v =
          if (i == 1) s.toVec.toSeq.sorted
          else (0 until s.length).map(_.toDouble)
        percentile(v, qs).toVector
      }
    }

  def dataSourceFromRowMajorVec(vec: Vec[Double], numCols: Int): DataTable =
    new DataTable(vec.contents, numCols)

  implicit def dataSourceFromZippedVecs2(
      vec1: (Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2))

  implicit def dataSourceFromZippedVecs3(
      vec1: (Vec[Double], Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2, vec1._3))

  implicit def dataSourceFromZippedVecs4(
      vec1: (Vec[Double], Vec[Double], Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2, vec1._3, vec1._4))

  implicit def dataSourceFromFrame[RX, CX](
      frame: Frame[RX, CX, Double]
  ): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {

      def iterator =
        frame.toRowSeq.map {
          case (rx, series) =>
            VectorRow(series.toVec.toSeq.toVector, rx.toString)
        }.iterator

      def dimension = frame.numCols

      override def columnNames = frame.colIx.toSeq.map(_.toString)
      override def columnMinMax(i: Int): Option[MinMax] = {
        if (i < frame.numCols) {
          val v = frame.colAt(i).toVec
          for {
            min <- v.min
            max <- v.max
          } yield {
            MinMaxImpl(min, max)
          }
        } else None
      }

      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        val v = frame.colAt(i).toVec
        percentile(v.toSeq, qs).toVector

      }
    }

  def density(
      data: Vec[Double],
      bandwidth: Double = 0.0,
      n: Int = 50
  ): DataSourceWithQuantiles = {
    val min1 = data.min.getOrElse(0d)
    val max1 = data.max.getOrElse(0d)
    val w1 = max1 - min1
    val min = min1 - 0.1 * w1
    val max = max1 - 0.1 * w1
    val w = (max - min) / n
    val h =
      if (bandwidth <= 0.0)
        1.06 * data.sampleStandardDeviation * math.pow(n.toDouble, -0.2)
      else bandwidth

    0 to n map { i =>
      val x = min + i * w
      x -> KDE.univariate(data.toArray, x, h)
    }
  }

  def densityPlot[F: FC](
      data: Frame[_, String, Double],
      lineWidth: RelFontSize = 0.2 fts,
      bandwidth: Double = 0d,
      n: Int = 50
  )(
      pars: CommonParameters = par()
  ) = {
    val colors = data.colIx.toSeq.distinct.zipWithIndex.toMap
    xyplot(
      data.toColSeq.map {
        case (name, col) =>
          (
            density(col.toVec.dropNA, bandwidth = bandwidth, n = n),
            line(
              stroke = Stroke(lineWidth),
              color = colorPick(colors(name), colors.size)
            ),
            InLegend(name)
          )
      }: _*
    )(pars)
  }

  def hist[F: FC](
      data: Frame[_, String, Double],
      breaks: Int = 50,
      opacity: Int = 255,
      bar: Boolean = true,
      relative: Boolean = false
  )(
      pars: CommonParameters = par()
  ) =
    org.nspl.histFromHistogramData(
      data.toColSeq.map {
        case (colIx, col) =>
          val hst = SaddleHistogramData.fromDataAndBreaks(col.toVec, breaks)
          (if (relative) hst.relative else hst) -> colIx
      },
      opacity,
      bar
    )(pars)
}
