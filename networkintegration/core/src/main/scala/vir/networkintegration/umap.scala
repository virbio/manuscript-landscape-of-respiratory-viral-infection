package vir.networkintegration

import org.saddle._
import org.saddle.order._
import smile.neighbor.NearestNeighborSearch
import lamp.CudaDevice
import lamp.CPU
import org.saddle.index.InnerJoin

object Umap {

  def plotWithColorScale(
      layout: Frame[String, Int, Double],
      edgeWeights: Mat[Double],
      nodeWeights: Series[String, Double],
      includeEdges: Boolean,
      includeGeneNames: Boolean,
      zeroClusterIsSmall: Boolean,
      withoutZeroCluster: Boolean,
      includeGeneNamesAboveNodeWeight: Double,
      includeGeneNamesSet: Set[String]
  ) = {
    import org.nspl._
    import org.nspl.saddle._
    import org.nspl.awtrenderer._
    case class ColorScale(
        max: Double = 1.0,
        white: Int = 255
    ) extends Colormap {

      def apply(value: Double): Color = {
        if (value.isNaN) Color.transparent
        else {

          val midColor = Color(white, white, white)

          if (value > 0) {
        
         
            if (value <= 1) colorList(0)
            else if (value <= 2) colorList(3)
            else if (value <= 3) colorList(2)
            else colorList(1) 
        
          } else Color.gray5
        }
      }

      def withRange(min: Double, max: Double) = ColorScale(max, white)
    }

    val umap = layout

    val sections = Mat(
      edgeWeights.rows
        .map { row =>
          val v1 = row.raw(0).toInt
          val v2 = row.raw(1).toInt
          val weight = row.raw(2)
          val v1Coord = layout.rowAt(v1).toVec
          val v2Coord = layout.rowAt(v2).toVec
          Vec(
            v1Coord.raw(0),
            v1Coord.raw(1),
            v2Coord.raw(0),
            v2Coord.raw(1),
            weight
          )
        }: _*
    ).T

    val plot = xyplot(
      (if (includeEdges)
         List(
           dataSourceFromMat(sections) -> lineSegment(
             color = GrayScale(min = 0d, max = 1d, white = 250)
               .mapColors(_.copy(a = 130)),
             colorCol = 4,
             strokes = Stroke(lineWidth * 0.1) :: Nil
           )
         )
       else Nil) ++
        List(
          dataSourceFromFrame(
            umap
              .addCol(
                nodeWeights,
                2,
                InnerJoin
              )
              .addCol(
                nodeWeights.mapValues(math.sqrt),
                3,
                InnerJoin
              )
          ) -> point(
            sizeCol = 3,
            size = (0.1 fts).value,
            labelText = includeGeneNames,
            labelFontBold = true,
            labelFontSize =
              if (umap.rowIx.toSeq.count(_.nonEmpty) > 70) 0.01 fts
              else 0.45 fts,
            forceDirectedLabelLayout = true,
            labelConnectionLineColor = Color(230, 216, 167),
            color = ColorScale(max = nodeWeights.max.get),
            shapeCol = 10
          ),
          dataSourceFromFrame(
            umap
              .addCol(
                nodeWeights,
                2,
                InnerJoin
              )
              .addCol(
                nodeWeights.mapValues(math.sqrt),
                3,
                InnerJoin
              )
          .toRowSeq.filter{ case (ix,row) => row.at(2).get > includeGeneNamesAboveNodeWeight || includeGeneNamesSet.contains(ix)}.toFrame.T) -> point(
            sizeCol = 3,
            size = (0.1 fts).value,
            labelText = true,
            labelFontSize =
              0.2 fts,
            forceDirectedLabelLayout = true,
            labelConnectionLineColor = Color(230, 216, 167),
            color = ColorScale(max = nodeWeights.max.get),
            shapeCol = 10
          )
         
        ): _*
    )(
      par(
        xgrid = false,
        ygrid = false,
        xNoTickLabel = true,
        yNoTickLabel = true,
        yNumTicks = 0,
        xNumTicks = 0,
        frame = false,
        xLineWidthFraction = 0d,
        yLineWidthFraction = 0d,
        rightPadding = 3 fts,
        leftPadding = 3 fts,
        extraLegend = List(
          ("1 pathogen" -> PointLegend(Shape.circle(1d),colorList(0))),
          ("2 pathogens" -> PointLegend(Shape.circle(1d),colorList(3))),
          ("3 pathogens" -> PointLegend(Shape.circle(1d),colorList(2))),
          ("4 pathogens" -> PointLegend(Shape.circle(1d),colorList(1))),
               
        

        ),
        legendToPlotLayout = VerticalStack(Anchor)
      )
    )
    group(ShapeElem(Shape.circle(25d),fill=Color.transparent),plot.build,VerticalStack())
  }

  def umapLamp(
      data: Frame[String, String, Double],
      dim: Int = 2,
      iterations: Int = 5000,
      device: lamp.Device = lamp.CudaDevice(0)
  ): (Frame[String, Int, Double], Mat[Double]) = {
    aten.Tensor.manual_seed(2354754L)
    val (projection, edgeWeights, _) = lamp.umap.Umap.umap(
      data = data.toMat,
      numDim = dim,
      // logger = Some(scribe.Logger("umap")),
      knnMinibatchSize = 100,
      k = math.min(data.numRows,15),
      device = device,
      iterations = iterations,
      negativeSampleSize = 10
    )

    val f = projection.toFrame

    (f.setRowIndex(data.rowIx), edgeWeights)

  }
}
