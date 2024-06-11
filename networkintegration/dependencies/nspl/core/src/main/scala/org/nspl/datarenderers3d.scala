package org.nspl

import data._
import scala.util.Try

trait DataRenderer3D {
  def render[R <: RenderingContext[R]](
      data: Row,
      ctx: R,
      viewProjectionMatrix: Math3D.Mat4,
      cameraPosition: Math3D.Vec3,
      cameraTarget: Math3D.Vec3,
      cameraUp: Math3D.Vec3,
      tx: AffineTransform,
      descriptor: Descriptor
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  def clear[R <: RenderingContext[R]](ctx: R)(
      implicit
      re: Renderer[ShapeElem, R],
      rt: Renderer[TextBox, R]
  ): Unit = ()
  def asLegend: List[(String, LegendElem)] = Nil
}

trait Renderers3D {
  def lineSegment3D[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      x2Col: Int = 3,
      y2Col: Int = 4,
      z2Col: Int = 5,
      colorCol: Int = 6,
      stroke: Stroke = Stroke(lineWidth * 0.01, CapRound),
      color: Colormap = HeatMapColors(0, 1),
      noDescriptor: Boolean = false
  ) = new DataRenderer3D {

    def render[R <: RenderingContext[R]](
        data: Row,
        ctx: R,
        viewProjectionMatrix: Math3D.Mat4,
        cameraPosition: Math3D.Vec3,
        cameraTarget: Math3D.Vec3,
        cameraUp: Math3D.Vec3,
        tx: AffineTransform,
        descriptor: Descriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol).toFloat
      val wY = data(yCol).toFloat
      val wZ = data(zCol).toFloat
      val wX2 = data(x2Col).toFloat
      val wY2 = data(y2Col).toFloat
      val wZ2 = data(z2Col).toFloat

      val matrix1 =
        Math3D.translate(viewProjectionMatrix, wX, wY, wZ)
      val matrix2 =
        Math3D.translate(viewProjectionMatrix, wX2, wY2, wZ2)

      val modelCoord = Math3D.Vec4(0f, 0f, 0f, 1f)
      val c1 =
        Math3D.vectorMultiply(modelCoord, matrix1)
      val c2 =
        Math3D.vectorMultiply(modelCoord, matrix2)

      if (c1(3) > 0 && c2(3) > 0) {

        val clip1 = Math3D.perspectiveDivide(c1)
        val clip2 = Math3D.perspectiveDivide(c2)

        val inbounds = clip1(0) > -1d &&
          clip1(0) < 1d &&
          clip1(1) < 1d &&
          clip1(1) > -1d &&
          clip2(0) > -1d &&
          clip2(0) < 1d &&
          clip2(1) < 1d &&
          clip2(1) > -1d

        if (inbounds) {
          val color1 =
            if (data.dimension > colorCol) color(data(colorCol)) else color(0d)
          val shape1 = ShapeElem(
            Shape.line(clip1.toPoint, clip2.toPoint),
            strokeColor = color1,
            stroke = Some(stroke)
          ).transform(_ => tx)
            .withDescriptor(if (noDescriptor) NoDescriptor else descriptor)

          re.render(ctx, shape1)
        }
      }

    }
  }
  def point3D[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      colorCol: Int = 3,
      sizeCol: Int = 4,
      size: Double = 3d,
      stroke: Stroke = Stroke(lineWidth),
      strokeWidth: Option[Double] = None,
      strokeColor: Option[Color] = None,
      color: Colormap = HeatMapColors(0, 1),
      shape: Shape = shapeList(0),
      keepPointShapeAspectRatio: Boolean = true,
      noDescriptor: Boolean = false
  ) = new DataRenderer3D {

    val zbuffer = scala.collection.mutable
      .ArrayBuffer[
        (Math3D.Vec3, Double, AffineTransform, Descriptor, Double)
      ]()

    override def clear[R <: RenderingContext[R]](ctx: R)(
        implicit re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      zbuffer.sortBy(v => -1 * v._1.z).foreach {
        case (clip1, dataColorValue, tx, descriptor, size1) =>
          val color1 = color(dataColorValue)

          val shapeBounds = shape.bounds
          val shapeAspectRatio =
            if (keepPointShapeAspectRatio) shapeBounds.h / shapeBounds.w
            else 1d

          val factorX = size1 / shapeBounds.w
          val factorY = shapeAspectRatio * size1 / shapeBounds.h

          val vX = clip1(0)
          val vY = clip1(1)
          val shape1PreTransform: ShapeElem = ShapeElem(
            shape.transform(
              _ =>
                AffineTransform
                  .translate(vX, vY)
                  .concatScale(
                    factorX,
                    factorY
                  )
            ),
            fill = color1,
            strokeColor = strokeColor.getOrElse(Color.black),
            stroke = strokeWidth.map(width => Stroke(width fts))
          )

          val shape1 = shape1PreTransform
            .transform(b => tx)
            .withDescriptor(if (noDescriptor) NoDescriptor else descriptor)

          re.render(ctx, shape1)
      }
      zbuffer.clear
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        ctx: R,
        viewProjectionMatrix: Math3D.Mat4,
        cameraPosition: Math3D.Vec3,
        cameraTarget: Math3D.Vec3,
        cameraUp: Math3D.Vec3,
        tx: AffineTransform,
        descriptor: Descriptor = NoDescriptor
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol).toFloat
      val wY = data(yCol).toFloat
      val wZ = data(zCol).toFloat
      val size1 = if (data.dimension > sizeCol) data(sizeCol) else size
      val matrix1 =
        Math3D.translate(viewProjectionMatrix, wX, wY, wZ)

      val modelCoord = Math3D.Vec4(0f, 0f, 0f, 1f)
      val c1 = Math3D.vectorMultiply(modelCoord, matrix1)

      if (c1(3) > 0) {

        val clip1 =
          Math3D.perspectiveDivide(c1)
        val inbounds = clip1(0) > -1d &&
          clip1(0) < 1d &&
          clip1(1) < 1d &&
          clip1(1) > -1d &&
          clip1(2) > -1d &&
          clip1(2) < 1d

        if (inbounds) {
          val dataColorValue =
            if (data.dimension > colorCol) data(colorCol) else 0d
          zbuffer.append((clip1, dataColorValue, tx, descriptor, size1))

        }
      }

    }
  }
}
