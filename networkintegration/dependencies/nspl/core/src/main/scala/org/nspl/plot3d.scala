package org.nspl

import data._

case class DataElem3D(
    data: DataSource,
    renderers: Seq[DataRenderer3D],
    matrix: Math3D.Mat4,
    cameraPosition: Math3D.Vec3,
    cameraTarget: Math3D.Vec3,
    cameraUp: Math3D.Vec3,
    originalBounds: Bounds = Bounds(-1, -1, 2, 2),
    tx: AffineTransform = AffineTransform.identity,
    idx: DataSourceIdx
) extends Renderable[DataElem3D] {
  def transform(tx: Bounds => AffineTransform) = {
    val ntx = tx(bounds).concat(this.tx)
    this.copy(tx = ntx)
  }
  def bounds = tx.transform(originalBounds)
}

object DataElem3D {
  implicit def dataElemRenderer[RC <: RenderingContext[RC]](
      implicit
      re: Renderer[ShapeElem, RC],
      rt: Renderer[TextBox, RC]
  ) = new Renderer[DataElem3D, RC] {
    def render(r: RC, e: DataElem3D): Unit = {
      e.data.iterator.zipWithIndex.foreach {
        case (row, rowIdx) =>
          e.renderers.foreach { dr =>
            dr.render(
              row,
              r,
              e.matrix,
              e.cameraPosition,
              e.cameraTarget,
              e.cameraUp,
              e.tx,
              DataRowIdx(e.idx.externalDataSourceIdx, e.idx.i, rowIdx)
            )
          }
      }
      e.renderers.foreach(_.clear(r))
    }
  }
}

trait Plots3D {
  import Math3D._

  type T =
    org.nspl.Elems3[org.nspl.ShapeElem, org.nspl.Elems3[
      org.nspl.ShapeElem,
      org.nspl.Elems4[org.nspl.ShapeElem, org.nspl.ShapeElem, org.nspl.ElemList[
        org.nspl.DataElem3D
      ], org.nspl.ElemOption[org.nspl.ShapeElem]],
      org.nspl.ShapeElem
    ], org.nspl.ShapeElem]

  case class XYZPlotArea(
      elem: Option[T],
      cameraPosition: Vec3,
      cameraTarget: Vec3,
      cameraUp: Vec3,
      worldTransformMatrix: Mat4,
      selectionMarker: Option[(Point, Point)]
  ) extends Renderable[XYZPlotArea] {
    def transform(v: Bounds => AffineTransform) =
      this.copy(elem = elem.map(_.transform(v)))
    def bounds: Bounds = elem.map(_.bounds).getOrElse(Bounds(0, 0, 0, 0))
  }

  object XYZPlotArea {
    implicit def renderer[RC <: RenderingContext[RC]](
        implicit
        re: Renderer[ShapeElem, RC],
        rt: Renderer[TextBox, RC]
    ) = new Renderer[XYZPlotArea, RC] {
      def render(r: RC, e: XYZPlotArea): Unit =
        e.elem.foreach(e => implicitly[Renderer[T, RC]].render(r, e))
    }
  }

  def xyzplotareaBuild[F: FC](
      data: Seq[(DataSource, List[DataRenderer3D])],
      externalDataSourceIdx: Int,
      zNear: Double,
      zFar: Double,
      fieldOfViewAngles: Double,
      cameraPosition: Vec3,
      cameraTarget: Vec3,
      cameraUp: Vec3,
      topPadding: RelFontSize,
      bottomPadding: RelFontSize,
      leftPadding: RelFontSize,
      rightPadding: RelFontSize,
      xWidth: RelFontSize,
      yHeight: RelFontSize,
      dragFactor: Double,
      scrollFactor: Double,
      areaId: Option[String]
  ) = {
    val id = areaId.getOrElse(new Object)
    Build(
      xyzplotarea(
        id,
        externalDataSourceIdx,
        data,
        zNear,
        zFar,
        fieldOfViewAngles,
        cameraPosition,
        cameraTarget,
        cameraUp,
        Math3D.identity,
        topPadding,
        bottomPadding,
        leftPadding,
        rightPadding,
        xWidth,
        yHeight
      )
    ) {
      case (Some(old), BuildEvent, _) =>
        xyzplotarea(
          id,
          externalDataSourceIdx,
          data,
          zNear,
          zFar,
          fieldOfViewAngles,
          old.cameraPosition,
          old.cameraTarget,
          old.cameraUp,
          Math3D.identity,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xWidth,
          yHeight,
          selectionMarker = None
        )
      case (Some(old), Scroll(v1, p, plotAreaId), replay)
          if plotAreaId.id == id =>
        val stepLength = math.abs(zFar - zNear) * scrollFactor
        val sub = subtractVectors(old.cameraTarget, old.cameraPosition)
        val distanceToTarget = sub.length
        val step =
          (if (v1 > 0) -stepLength else if (v1 < 0) stepLength else 0.0)

        val cameraDirection =
          normalize(sub)
        val newCameraPosition = old.cameraPosition + cameraDirection * (step.toFloat)

        if (replay)
          XYZPlotArea(
            None,
            newCameraPosition,
            old.cameraTarget,
            old.cameraUp,
            old.worldTransformMatrix,
            None
          )
        else
          xyzplotarea(
            id,
            externalDataSourceIdx,
            data,
            zNear,
            zFar,
            fieldOfViewAngles,
            newCameraPosition,
            old.cameraTarget,
            old.cameraUp,
            old.worldTransformMatrix,
            topPadding,
            bottomPadding,
            leftPadding,
            rightPadding,
            xWidth,
            yHeight,
            selectionMarker = None
          )

      case (Some(old), Drag(dragStart, dragTo, plotAreaId), replay)
          if plotAreaId.id == id =>
        val dX = (dragTo.x - dragStart.x) * dragFactor * (-1)
        val dY = (dragTo.y - dragStart.y) * dragFactor * (-1)

        val xAngle = dY * 0.1
        val yAngle = dX * -0.1

        val localZAxis =
          normalize(subtractVectors(old.cameraPosition, old.cameraTarget))
        val localXAxis = normalize(cross(old.cameraUp, localZAxis))
        val localYAxis = normalize(cross(localZAxis, localXAxis))

        val xRotation = Math3D.arbitraryRotation(yAngle.toFloat, localYAxis)
        val yRotation = Math3D.arbitraryRotation(xAngle.toFloat, localXAxis)

        val rotationOfObject = multiply(
          multiply(yRotation, xRotation),
          old.worldTransformMatrix
        )
        if (replay)
          XYZPlotArea(
            None,
            old.cameraPosition,
            old.cameraTarget,
            old.cameraUp,
            rotationOfObject,
            None
          )
        else
          xyzplotarea(
            id,
            externalDataSourceIdx,
            data,
            zNear,
            zFar,
            fieldOfViewAngles,
            old.cameraPosition,
            old.cameraTarget,
            old.cameraUp,
            rotationOfObject,
            topPadding,
            bottomPadding,
            leftPadding,
            rightPadding,
            xWidth,
            yHeight,
            selectionMarker = None
          )
      case (Some(old), Selection(dragStart, dragTo, plotAreaDescriptor), replay)
          if plotAreaDescriptor.id == id =>
        val dragStartScreen: Point =
          mapPoint(
            dragStart,
            plotAreaDescriptor.bounds.get,
            Bounds(-1, -1, 2d, 2d),
            true
          )
        val dragToScreen: Point = mapPoint(
          dragTo,
          plotAreaDescriptor.bounds.get,
          Bounds(-1, -1, 2d, 2d),
          true
        )
        if (replay)
          XYZPlotArea(
            None,
            old.cameraPosition,
            old.cameraTarget,
            old.cameraUp,
            old.worldTransformMatrix,
            Some((dragStartScreen, dragToScreen))
          )
        else
          xyzplotarea(
            id,
            externalDataSourceIdx,
            data,
            zNear,
            zFar,
            fieldOfViewAngles,
            old.cameraPosition,
            old.cameraTarget,
            old.cameraUp,
            old.worldTransformMatrix,
            topPadding,
            bottomPadding,
            leftPadding,
            rightPadding,
            xWidth,
            yHeight,
            selectionMarker = Some((dragStartScreen, dragToScreen))
          )
    }
  }

  def xyzplotarea[F: FC](
      id: AnyRef,
      externalDataSourceIdx: Int,
      data: Seq[(DataSource, List[DataRenderer3D])],
      zNear: Double = 1d,
      zFar: Double = 2000d,
      fieldOfViewAngles: Double = 60,
      cameraPosition: Vec3 = Vec3(50f, 50f, 300f),
      cameraTarget: Vec3 = Vec3(0f, 0f, 0f),
      cameraUp: Vec3 = Vec3(0, 1, 0),
      worldRotationAroundCenter: Mat4 = Math3D.identity,
      topPadding: RelFontSize = 0d fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0d fts,
      xLabDistance: RelFontSize = 0.5 fts,
      yLabDistance: RelFontSize = 0.5 fts,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      selectionMarker: Option[(Point, Point)] = None
  ) = {

    val aspect = xWidth.value / yHeight.value

    val fieldOfViewRadians = degToRad(fieldOfViewAngles)
    val projectionMatrix = perspective(
      fieldOfViewRadians,
      aspect.toFloat,
      zNear.toFloat,
      zFar.toFloat
    )

    val viewMatrix = multiply(
      lookAt(cameraPosition, cameraTarget, cameraUp),
      Math3D
        .translation(-cameraPosition.x, -cameraPosition.y, -cameraPosition.z)
    )
    val viewProjectionMatrix =
      multiply(projectionMatrix, viewMatrix)

    val center = cameraTarget
    val trBackFromCenter = Math3D.translation(center.x, center.y, center.z)
    val trIntoCenter = Math3D.translation(-center.x, -center.y, -center.z)
    val viewProjectionUIRotation = multiply(
      viewProjectionMatrix,
      multiply(
        trBackFromCenter,
        multiply(worldRotationAroundCenter, trIntoCenter)
      )
    )

    val fakeAxis = LinearAxisFactory.make(0, 0, 0, false, false)

    val clipFrame = ShapeElem(
      Shape.rectangle(-1, -1, 2, 2), // (-1,1) the screen bounds, see opengl lessons
      stroke = Some(Stroke(lineWidth * 0.01)),
      fill = Color.transparent,
      strokeColor = Color.black
    ).withDescriptor(PlotAreaDescriptor(id, fakeAxis, fakeAxis, None, false))

    val dataelem = sequence(data.toList.zipWithIndex.map {
      case ((ds, drs), idx) =>
        DataElem3D(
          ds,
          drs,
          viewProjectionUIRotation,
          cameraPosition,
          cameraTarget,
          cameraUp,
          idx = DataSourceIdx(externalDataSourceIdx, idx)
        )
    })

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
    val crosshair = ShapeElem(
      shape = Shape.circle(0.01),
      fill = Color.gray1,
      stroke = None
    )

    val selectionRectangle = ElemOption(selectionMarker.map {
      case (fromW, toW) =>
        val x1 = (fromW.x)
        val x2 = (toW.x)
        val y1 = (fromW.y)
        val y2 = (toW.y)
        val x = math.min(x1, x2)
        val y = -math.max(y1, y2)
        val w = math.abs(x1 - x2)
        val h = math.abs(y1 - y2)
        ShapeElem(
          shape = Shape.rectangle(x, y, w, h),
          fill = Color.transparent,
          strokeColor = Color.gray2,
          stroke = Some(Stroke(lineWidth * 0.01, dashArray = List(0.01d)))
        )
    })

    val plotWithFrame =
      group(crosshair, clipFrame, dataelem, selectionRectangle, FreeLayout)

    val elem = group(
      padTop,
      group(
        padLeft,
        plotWithFrame,
        padRight,
        HorizontalStack(Center, 0d fts)
      ),
      padBottom,
      VerticalStack(Center, 0d fts)
    )

    fitToBounds(
      XYZPlotArea(
        Some(elem),
        cameraPosition,
        cameraTarget,
        cameraUp,
        worldRotationAroundCenter,
        selectionMarker
      ),
      Bounds(0, 0, xWidth.value, yHeight.value)
    )

  }

}
