package org.nspl
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import org.nspl.saddle._
import org.nspl.data._
import org.nspl.awtrenderer._
import org.saddle._
import org.saddle.order._

import org.saddle.csv._

class SaddlePlotSpec extends AnyFunSpec with Matchers {

  implicit val myfont = font("Arial")

  def readFrameFromClasspath[T:ST:ORD](s: String) : Frame[Int,String,T]=
    CsvParser
      .parseInputStreamWithHeader[T](
        getClass.getResourceAsStream(s),
        recordSeparator = "\n"
      )
      .right
      .get
      // .withColIndex(0).mapColIndex(_.toString)

  describe("plots") {

    it("plot gallery") {

      val radar =
        radarPlot(
          data = List((0d, 1d, 3d, 5d, 0d), (1d, 2d, 4d, 6d, 1d)),
          metaData = List(0d, 0d),
          colorCol = 0,
          lineColors = Color.red,
          strokeCol = 0
        )(par())

      val evec =
        readFrameFromClasspath[Double]("/evec.csv")
      val rotated = readFrameFromClasspath[Double]("/rotated.csv")
      
      val data = readFrameFromClasspath[Double]("/data.csv")
        .colAt(Array(0, 1, 2, 3))
      val species : Series[Int,String] = readFrameFromClasspath[String]("/data.csv").colAt(4)
      val spec2Num = species.toVec.toSeq.distinct.sorted.zipWithIndex.toMap
      val spec: Series[Int, Double] =
        species.mapValues(spec2Num).mapValues(_.toDouble)

      val eval = readFrameFromClasspath[Double]("/sqrteigen.csv")
        .mapValues(x => x * x)

      val data2 = data.addCol(spec, "spec")

      val scree = xyplot(
        (
          indexed(eval.firstCol("x").toVec.toSeq.sorted.reverse),
          List(line()),
          InLegend("scree")
        )
      )(
        par(xAxisMargin = 0, xlab = "Order", ylab = "Eval", main = "Scree")
      )

      val empty = xyplot()(par())

      val hist1 = xyplot(
        HistogramData(rotated.firstCol("PC1").toVec.toSeq, 10) -> bar()
      )(
        par(
          xlab = "PC1",
          ylab = "freq.",
          main = "Loading distribution",
          ylim = Some(0d, Double.NaN)
        )
      )

      val hist2 = xyplot(
        HistogramData(
          rotated.firstCol("PC1").toVec.toSeq,
          Seq(0d -> 1d, 1d -> 2d, 2d -> 3d, 3d -> 4d, 4d -> 5d)
        ) -> bar()
      )(
        par(
          xlab = "PC1",
          ylab = "freq.",
          main = "Loading distribution",
          ylim = Some(0d, Double.NaN)
        )
      )

      val hist3 = xyplot(
        HistogramData(
          rotated.firstCol("PC1").toVec.toSeq,
          step = 1d,
          min = 0d,
          max = None
        ) -> bar()
      )(
        par(
          xlab = "PC1",
          ylab = "freq.",
          main = "Loading distribution",
          ylim = Some(0d, Double.NaN)
        )
      )

      val bar1 = barplotVertical(
        Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d),
        color = Gradient3.greyBrown(-2, 2)
      )(par())
      val bar2 = barplotHorizontal(
        Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d)
      )(par())

      val density1 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line(
          stroke = Stroke(lineWidth)
        )
      )(par(xlab = "PC1", ylab = "dens.", main = "Loading distribution"))

      val density3 = org.nspl.saddle.densityPlot(rotated)(
        par(xlab = "PC1", ylab = "dens.", main = "Loading distribution")
      )
      val hist4 = org.nspl.saddle.hist(rotated, opacity = 80)(
        par(xlab = "PC1", ylab = "dens.", main = "Loading distribution")
      )
      val hist5 = org.nspl.saddle.hist(rotated, opacity = 80, bar = false)(
        par(xlab = "PC1", ylab = "dens.", main = "Loading distribution")
      )

      val density_noYTick = group(
        TextBox("abc"),
        TextBox("def"),
        xyplot(
          density(rotated.firstCol("PC1").toVec.toSeq) -> line(
            stroke = Stroke(lineWidth)
          )
        )(
          par(
            xlab = "PC1",
            ylab = "dens.",
            main = "Loading distribution",
            rightPadding = 0 fts
          )
        ),
        xyplot(
          density(rotated.firstCol("PC1").toVec.toSeq) -> line(
            stroke = Stroke(lineWidth)
          )
        )(
          par(
            xlab = "PC1",
            main = "Loading distribution",
            yNoTickLabel = true,
            leftPadding = 0 fts
          )
        ),
        TableLayout(2, horizontalGap = lineWidth * (-0.5))
      )

      val r1 = rasterplotFromFrame(
        rotated,
        
        xTickLabelColors = Vector(Color.red)
      )(par(yLabFontSize = 0.1 fts))

      val fig0 = xyplot(
        data2.col("Sepal.Length", "Sepal.Width", "spec")
      )(
        par(
          extraLegend = spec2Num.toSeq.map(
            x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
          ),
          xlab = "Sepal.Length",
          ylab = "Sepal.Width",
          main = "Iris data"
        ),
        xSecondaryData = List(
          dataSourceFromFrame(
            data2.col("Sepal.Length", "Sepal.Width", "spec")
          ) -> List(point())
        )
      )

      val fig0ExtraShapes = xyplot(
        data2.col("Sepal.Length", "Sepal.Width", "spec") -> point(
          color = Color.transparent,
          strokeColor = Some(Color.red),
          strokeWidth = Some(0.05d),
          polygonTriangles = List(
            (Color.black, 4),
            (Color.red, 4),
            (Color.blue, 4),
            (Color.green, 4)
            // (Color.blue, 4),
            // (Color.blue, 4)
          )
        )
      )(
        par(
          extraLegend = spec2Num.toSeq.map(
            x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
          ),
          xlab = "Sepal.Length",
          ylab = "Sepal.Width",
          main = "Iris data"
        )
      )

      val fig1 = xyplot(
        density2d(
          rotated.firstCol("PC1").toVec.toSeq zip rotated
            .firstCol("PC2")
            .toVec
            .toSeq
        ),
        Frame((rotated.col("PC1", "PC2").toColSeq :+ ("spec" -> spec)): _*) -> point(
          valueText = true,
          forceDirectedLabelLayout = true
        )
      )(
        par(
          extraLegend = spec2Num.toSeq.map(
            x =>
              x._1.toUpperCase -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
          ),
          xlab = "PC1",
          ylab = "PC2",
          main = "PC1 vs PC2",
          legendFontSize = 0.2d fts
        )
      )

      val bezierElem = ShapeElem(
        Path(
          List(
            MoveTo(Point(0.0, 0.0)),
            CubicTo(
              Point(100.0, 0.0),
              Point(0d, 50d),
              Point(200d, -100d)
            )
          ),
          AffineTransform.identity
        )
      )

      val bezier = xyplot(
        List((0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0)) -> cubicBezier()
      )(par())

      val lineSegmentWithText = xyplot(
        List(
          VectorRow(
            Vector(0.0, 1.0, 1.0, 2.0, 3d),
            "label (0,1) -> (1,2)"
          )
        ) -> lineSegment(
          labelText = true,
          arrowForward = true,
          arrowBackward = true,
          offsetCol = 4
        )
      )(par())

      val densmap = rasterplot(
        densityMatrix(
          rotated.firstCol("PC1").toVec.toSeq zip rotated
            .firstCol("PC2")
            .toVec
            .toSeq
        ),
        colormap = RedBlue(white = 200),
        zlab = "density"
      )(
        par(xgrid = false, ygrid = false, xSecondaryYVisible = false),
        List(indexed(List(1d, 2d, 3d)) -> List(point()))
      )

      val sparseRasterEmpty = rasterplot(
        SparseDataMatrix(Vector.empty, 4, 3, 0d)
      )(par(noLegend = true))
      val sparseRasterDiagonal = rasterplot(
        SparseDataMatrix.fromUnsorted(
          Vector(
            (0, 0, 1d),
            (0, 3, 2d),
            (1, 1, 1d),
            (2, 2, 1d),
            (2, 3, 2d),
            (3, 3, 2d)
          ).reverse.map(v => (v._1 + 1, v._2 + 1, v._3)),
          5,
          4,
          0d
        )
      )(
        par(
          extraLegend = Seq("boo" -> PointLegend(Shape.circle(1d), Color.RED)),
          xgrid = false,
          ygrid = false
        )
      )

      val fig2 = xyplot(
        Frame((rotated.col("PC2", "PC3").toColSeq :+ ("spec" -> spec)): _*)
      )(
        par(
          extraLegend = spec2Num.toSeq.map(
            x =>
              x._1 -> PointLegend(
                shape = Shape.rectangle(0, 0, 1, 1),
                color = DiscreteColors(spec2Num.size)(x._2)
              )
          ),
          xlab = "PC2",
          ylab = "PC3",
          main = "PC2 vs PC3"
        )
      )

      val pc3 = rotated.firstCol("PC3").toVec.toSeq
      val pc2 = rotated.firstCol("PC2").toVec.toSeq

      val fig3 = binnedboxplot(pc3, pc2)(par(xlab = "PC2", ylab = "PC3"))

      val fig4 = boxplotFromColumns(
        data2
          .firstCol("Sepal.Length")
          .toVec
          .toSeq -> data2.firstCol("Sepal.Width").toVec.toSeq,
        xnames = Seq("Sepal Length", "Sepal Width")
      )(par())

      val boxpl2 =
        boxplotFromGroupLabels(
          List("a" -> 0d, "a" -> 1d, "b" -> 3d, "b" -> 4d)
        )(
          par(main = "boxplotfromlabels")
        )
      val f = Frame(
        "a" -> Series("1" -> 1d, "2" -> 2d, "3" -> 3d),
        "b" -> Series("1" -> 5d, "2" -> 6d, "3" -> 7d)
      )
      val boxpl3 =
        boxplotFromColumns(
          f,
          boxWidth = 0.5,
          boxColor = ManualColor(Map(0d -> Color.red, 1d -> Color.blue))
        )(
          par(main = "boxplotfromframe")
        )

      val contour = contourplot(
        xlim = -2d -> 2d,
        ylim = -2d -> 2d,
        f = (x, y,_,_) => (x * x + y * y - 0.5 + x * y),
        n = 50,
        levels = 10
      )(par())

      val density2 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line()
      )(par(xlab = "PC1", ylab = "dens.", main = "Loading distribution"))

      val empty2 =
        xyplotarea(
          "id",
          0,
          Nil,
          AxisSettings(LinearAxisFactory),
          AxisSettings(LinearAxisFactory),
          origin = Some(Point(0.0, 0.0)),
          xlim = Some(-1d -> 1d),
          ylim = Some(-1d -> 1d),
          yAxisLabelRotation = 0d,
          yAxisLabelWidth = None
        )

      val rs = (1 to 99 map (i => scala.util.Random.nextGaussian)).toSeq :+ 1e3

      val p6 = rasterplot(
        rasterFromSeq(rs, 10, 10),
        valueText = true,
        valueColor = Color.white,
        valueFontSize = 0.3 fts,
        horizontalMarkers = Seq((5, Stroke(lineWidth), Color.black)),
        verticalMarkers = Seq((5, Stroke(lineWidth), Color.black))
      )(
        par(
          xLabFontSize = 0.5 fts,
          yLabFontSize = 0.1 fts,
          xnames = Seq(0.0 -> "sdfsf", 1.0 -> "dsfds adfdf adfs f"),
          ynames = Seq(0.0 -> "dsfsdf", 2.0 -> "dfsdf asdfdf asdfds sdfadsf"),
          main = "1main 2main main3 m4ain m5ain m6ain 7mian 8mian 9main ",
          mainFontSize = 1 fts
        )
      )

      val p6b = rasterplot(
        rasterFromSeq(rs, 10, 10),
        valueText = true,
        colormap = LogHeatMapColors(),
        valueColor = Color.white,
        valueFontSize = 0.3 fts
      )(
        par(
          xLabFontSize = 0.5 fts,
          yLabFontSize = 0.1 fts,
          xnames = Seq(0.0 -> "sdfsf", 1.0 -> "dsfds adfdf adfs f"),
          ynames = Seq(0.0 -> "dsfsdf", 2.0 -> "dfsdf asdfdf asdfds sdfadsf"),
          main = "1main 2main main3 m4ain m5ain m6ain 7mian 8mian 9main ",
          mainFontSize = 1 fts
        )
      )

      val barplot2 = {
        val dataraw
            : IndexedSeq[(Double, Double, Double, Double)] = 1 to 100 map (
            i =>
              (
                i.toDouble,
                scala.util.Random.nextInt(i).toDouble,
                scala.util.Random.nextInt(101 - i).toDouble,
                scala.util.Random.nextInt(50).toDouble
              )
          )

        stackedBarPlot(
          dataraw,
          List(
            (1, "red", Color.red),
            (2, "blue", Color.blue),
            (3, "green", Color.green)
          ),
          relative = true
        )(par())
      }

      val colortest60 = xyplot(
        indexed(0 to 60 map (_.toDouble) toSeq) -> point(
          color = DiscreteColors(60),
          colorCol = 1
        )
      )()
      val colortest8 = xyplot(
        indexed(0 to 60 map (_.toDouble) toSeq) -> point(
          color = DiscreteColors(8),
          colorCol = 1
        )
      )()
      val colortest16 = xyplot(
        indexed(0 to 60 map (_.toDouble) toSeq) -> point(
          color = DiscreteColors(16),
          colorCol = 1
        )
      )()
      val colortestColorList = xyplot(
        indexed(0 to 60 map (_.toDouble) toSeq) -> point(
          color = DiscreteColors(colorList.size),
          colorCol = 1
        )
      )()
      val ticktest = xyplot(
        indexed(
          Seq(0.001, 0.01, 0.0, -1.2, 1.1, 100.0, 100.001, 100.1, 1000d, 10000d)
        ) -> point()
      )()

      val ticktest2 =
        xyplot(indexed(Seq(100.0, 100.001, 100.1, 100.101234)) -> point())()
      val ticktest3 = xyplot(
        indexed(Seq(1e-5, 0.00003, 0.00002, 0.00001, 2.313e-6)) -> point()
      )()
      val ticktest4 = xyplot(
        indexed(Seq(1e-5, 0.00003, 0.00002, 0.00001, 2.313e-6, 1e6)) -> point()
      )()
      val ticktest5 = xyplot(indexed(Seq(1e6, 1.02e6, 2.567e7)) -> point())()
      val ticktest6 =
        xyplot(indexed(Seq(100.0, 100.001, 100.0015, 100.0016)) -> point())()

      val logaxistest =
        xyplot(
          Seq(
            1e-6 -> 1e-6,
            1e-5 -> 1e-5,
            1e-4 -> 1e-4,
            1e-3 -> 1e-3,
            1e-2 -> 1e-2,
            1e-1 -> 1e-1,
            1d -> 1d,
            10d -> 10d,
            1e2 -> 100d,
            1e3 -> 1000d,
            1e4 -> 10000d,
            1e5 -> 1e5,
            1e6 -> 1e6,
            1e7 -> 1e7
          ) -> point()
        )(par(ylog = true, xlog = true, xLabelRotation = -0.7))
      val logaxistest2 =
        xyplot(
          Seq(1.123e-6 -> 1e-6, 1e-5 -> 1e-5, 1e-4 -> 3.132e-4) -> point()
        )(par(ylog = true, xlog = true))
      val logaxistest3 =
        xyplot(
          Seq(0.92 -> 1d, 10d -> 10d, 1e2 -> 100d, 1e3 -> 1324d) -> point()
        )(par(ylog = true, xlog = true))
      val logaxistest4 =
        xyplot(
          Seq(0.92 -> 1d, 10d -> 10d, 1e2 -> 100d, 1e3 -> 1324d) -> point()
        )(par(ylog = true, xlog = true, xlim = Some(0.5 -> 1500d)))

      val hexbinTest = {
        val binning =
          hexbin(
            rotated.firstCol("PC1").toVec.toSeq zip rotated
              .firstCol("PC2")
              .toVec
              .toSeq,
            size = 0.2,
            color = GrayScale(0, 30)
          )
        xyplot(
          binning,
          Frame((rotated.col("PC1", "PC2").toColSeq :+ ("spec" -> spec)): _*) -> point(
            size = 1
          )
        )(
          par(
            extraLegend = spec2Num.toSeq.map(
              x =>
                x._1 -> PointLegend(
                  shape = Shape.rectangle(0, 0, 1, 1),
                  color = DiscreteColors(spec2Num.size)(x._2)
                )
            ),
            xlab = "PC1",
            ylab = "PC2",
            main = "PC1 vs PC2"
          )
        )
      }

      val gallery = group(
        group(
          fig0ExtraShapes,
          radar,
          scree,
          lineSegmentWithText,
          empty,
          ticktest,
          ticktest2,
          ticktest3,
          ticktest4,
          ticktest5,
          ticktest6,
          logaxistest,
          logaxistest2,
          logaxistest3,
          logaxistest4,
          colortest60,
          colortest8,
          colortest16,
          colortestColorList,
          TableLayout(3)
        ),
        bar1,
        // bar2,
        // boxpl2,
        // p6,
        // p6b,
        // empty2,
        // xyplot(Seq(0d -> 0d, 1d -> 1d, 2d -> 2d))(par()),
        // r1,
        // hist1,
        // hist2,
        // hist3,
        // contour,
        // density1,
        // fig0,
        // fig1,
        // hexbinTest,
        // densmap,
        fig2,
        fig3,
        group(
          density3,
          hist4,
          hist5,
          boxpl3,
          fig4,
          barplot2,
          density_noYTick,
          barplot2,
          sparseRasterEmpty,
          sparseRasterDiagonal,
          bezier,
          TextBox(
            "aabcd fafafafabcd fafafafabcd faafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd ffafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafabcd fafafafbcd fafafaf"
          ),
          TableLayout(2)
        ),
        ColumnLayout(4)
      )

      val text = fitToWidth(
        group(
          ShapeElem(Shape.circle(1)),
          TextBox("ABC abc def ghijklmn opqrstvuwxyz"),
          TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
            .translate(10, 30),
          TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
            .translate(10, 30)
            .rotate(math.Pi / 2, 0d, 0d),
          TextBox("abc def ghijklmn", fontSize = 0.1 fts).rotate(1d),
          TextBox("opqrstvuwxyz", fontSize = 0.1 fts).translate(10, 30),
          TextBox("abc def ghijklmn opqrstvuwxyz", fontSize = 1 fts)
            .translate(100, 30),
          FreeLayout
        ),
        200
      )

      {
        import awtrenderer._

        // show(gallery)
        // println(pngToFile(gallery.build))
        // show(bezierElem)
        println(pdfToFile(gallery.build))
        println(pngToFile(gallery.build, width = 15000))
        while (false) {
          gallery.build
        }
        // show(text)
        // println(pdfToFile(text))
        // println(renderToFile(gallery, 1000, "image/svg"))
      }
      {
        import scalatagrenderer._
        println(svgToFile(gallery.build))
      }
    }

  }
}
