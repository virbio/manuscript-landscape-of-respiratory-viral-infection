package org.nspl

import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.raw._
import org.scalajs.dom.html
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

case class CanvasRC(
    graphics: CanvasRenderingContext2D,
    hover: Option[(Descriptor, MouseEvent) => Unit],
    unhover: Option[(Descriptor, MouseEvent) => Unit],
    click: Option[(Descriptor, MouseEvent) => Unit],
    selection: Option[collection.Seq[Descriptor] => Unit]
) extends RenderingContext[CanvasRC] {

  var transform: AffineTransform = AffineTransform.identity
  var fillColor: Color = Color.black
  var strokeColor: Color = Color.black
  var dash: Seq[Double] = Nil
  var transformInGraphics: AffineTransform = AffineTransform.identity

  def withDash[T](d: Seq[Double])(f: => T) = {
    val current = dash
    if (current != d) {
      dash = d
      graphics.setLineDash(scalajs.js.Array.apply(d: _*))
    }
    f
  }
  def withFill[T](color: Color)(f: => T) = {
    val current = fillColor
    if (current != color) {
      fillColor = color
      graphics.fillStyle = canvasrenderer.asCss(color)
    }
    f
  }
  def withStroke[T](color: Color)(f: => T) = {
    val current = strokeColor
    if (current != color) {
      strokeColor = color
      graphics.strokeStyle = canvasrenderer.asCss(color)
    }
    f
  }

  type LocalTx = AffineTransform

  def localToScala(tx: AffineTransform): AffineTransform = tx

  def concatTransform(tx: AffineTransform): Unit = {
    transform = transform.concat(tx)
  }

  def setTransform(tx: LocalTx): Unit = {
    transform = tx
  }
  def setTransformInGraphics() = {
    if (transformInGraphics != transform) {
      transformInGraphics = transform
      graphics.setTransform(
        transform.m0,
        transform.m3,
        transform.m1,
        transform.m4,
        transform.m2,
        transform.m5
      )
    }
  }

  def getTransform: LocalTx = transform

  var mousedown = false

  val clickShapes = scala.collection.mutable.ArrayBuffer[(Shape, Descriptor)]()
  val hoverShapes = scala.collection.mutable.ArrayBuffer[(Shape, Descriptor)]()

  val selectionShapes =
    scala.collection.mutable.ArrayBuffer[(Shape, Descriptor)]()

  val plotAreaShapes =
    scala.collection.mutable.ArrayBuffer[(Shape, PlotAreaDescriptor)]()

  def registerOnSelection(shape: Shape, descr: Descriptor) =
    selectionShapes.append(shape -> descr)
  def registerOnClick(shape: Shape, descr: Descriptor) =
    clickShapes.append(shape -> descr)
  def registerOnHover(shape: Shape, descr: Descriptor) =
    hoverShapes.append(shape -> descr)
  def registerPlotArea(shape: Shape, descr: PlotAreaDescriptor) =
    plotAreaShapes.append(shape -> descr)

  var lastHovers: scala.collection.Seq[Int] = List.empty
  def callHoverCallbackOnHit(e: MouseEvent, p: Point) =
    hover.foreach(
      hover => {
        val currentHovers = lastHovers
        lastHovers =
          hitTest(e, p, false, hoverShapes.toSeq, (id, bounds) => hover(id, e))
        val unhovers = currentHovers.filterNot(i => lastHovers.contains(i))
        unhovers.foreach { idx =>
          unhover.foreach(unhover => unhover(hoverShapes(idx)._2, e))
        }

      }
    )
  def callClickCallbackOnHit(e: MouseEvent, p: Point) =
    click.foreach(
      click =>
        hitTest(e, p, false, clickShapes.toSeq, (id, bounds) => click(id, e))
    )
  def callSelectionCallback(
      e: MouseEvent,
      selection1: Point,
      selection2: Point
  ) = {
    selection.foreach { selection =>
      val selectionRectangle = Bounds.fromPoints(selection1, selection2)
      val selected = selectionShapes
        .filter {
          case (shape, _) =>
            val shapeBounds = shape.bounds
            val center = Point(shapeBounds.centerX, shapeBounds.centerY)
            val contains = selectionRectangle.contains(center)

            contains
        }
        .map { case (_, descriptor) => descriptor }
      selection(selected)
    }
  }

  def callIfPlotAreaIsHit(e: MouseEvent, p: Point)(
      cb: PlotAreaDescriptor => Unit
  ) =
    hitTest(
      e,
      p,
      true,
      plotAreaShapes.toSeq,
      (descr: PlotAreaDescriptor, bounds) => cb(descr.copy(bounds = bounds))
    )

  private def hitTest[T](
      e: MouseEvent,
      p: Point,
      needsTransformedBounds: Boolean,
      shapes: collection.Seq[(Shape, T)],
      callback: (T, Option[Bounds]) => Unit
  ): scala.collection.Seq[Int] = {
    import canvasrenderer._
    val ctx = graphics

    shapes.zipWithIndex
      .filter {
        case ((shape, id), shapeIndex) =>
          val (hit, transformedBounds) = withTransform(shape.currentTransform) {
            setTransformInGraphics()
            val transformedBounds =
              if (needsTransformedBounds)
                id match {
                  case pl: PlotAreaDescriptor =>
                    pl.bounds.map(transform.transform)
                  case _ => None
                } else None
            val bool = shape match {
              case Rectangle(x, y, w, h, tx, _) =>
                ctx.beginPath()
                ctx.rect(x, y, w, h)
                val r =
                  ctx.isPointInPath(p.x, p.y)

                r

              case Ellipse(x, y, w, h, tx) => {
                val centerX = x + .5 * w
                val centerY = y + .5 * h
                val radiusX = w * .5
                val radiusY = h * .5
                ctx.beginPath
                ctx
                  .asInstanceOf[scala.scalajs.js.Dynamic]
                  .ellipse(
                    centerX,
                    centerY,
                    radiusX,
                    radiusY,
                    0,
                    0,
                    2 * Math.PI
                  )
                val r = ctx.isPointInPath(p.x, p.y)
                r
              }
              case Line(x1, y1, x2, y2, _) => {
                ctx.beginPath
                ctx.moveTo(x1, y1)
                ctx.lineTo(x2, y2)
                val r = ctx.isPointInPath(p.x, p.y)
                r
              }
              case SimplePath(points, _) => {
                ctx.beginPath
                points.foreach { p =>
                  ctx.lineTo(p.x, p.y)
                }
                val r = ctx.isPointInPath(p.x, p.y)
                r
              }
              case Path(ops, _) => {
                ctx.beginPath
                ops foreach {
                  case MoveTo(Point(x, y)) => ctx.moveTo(x, y)
                  case LineTo(Point(x, y)) => ctx.lineTo(x, y)
                  case QuadTo(Point(x2, y2), Point(x1, y1)) =>
                    ctx.quadraticCurveTo(x1, y1, x2, y2)
                  case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) =>
                    ctx.bezierCurveTo(x1, y1, x2, y2, x3, y3)
                }
                val r = ctx.isPointInPath(p.x, p.y)
                r
              }
            }
            (bool, transformedBounds)

          }

          if (hit) {
            callback(id, transformedBounds)
            true
          } else false

      }
      .map(_._2)
  }

  def clear() = {
    graphics.setTransform(1, 0, 0, 1, 0, 0)
    transformInGraphics = AffineTransform.identity
    graphics.clearRect(0, 0, graphics.canvas.width, graphics.canvas.height)
    plotAreaShapes.clear()
    clickShapes.clear()
    hoverShapes.clear()
    selectionShapes.clear()
  }

}

object canvasrenderer {

  implicit val defaultGlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultFont: FontConfiguration = font("Arial")

  private[nspl] def rec2bounds(r: DOMRect) =
    Bounds(r.left, r.top, r.width, r.height)

  private[nspl] def cssColor(c: Color) = s"rgba(${c.r},${c.g},${c.b},${c.a}"

  private[nspl] def getCanvasCoordinate(
      canvas: html.Canvas,
      e: MouseEvent,
      devicePixelRatio: Double
  ): Point = {
    def rect = canvas.getBoundingClientRect()
    val x = e.clientX - rect.left
    val y = e.clientY - rect.top
    Point(x * devicePixelRatio, y * devicePixelRatio)
  }

  class BuildEventStore(
      private var buffer: Vector[Event]
  ) {
    def clear = buffer = Vector.empty
    def get = buffer
    def add(ev: Event, fusable: Boolean): Unit =
      if (fusable) {
        (buffer.lastOption, ev) match {
          // fuse two consecutive drags are two consecutive selections together
          // saves time on replay
          case (
              Some(Drag(start1, current1, area1)),
              Drag(start2, current2, area2)
              ) if area2.id == area1.id && current1 == (start2) =>
            buffer = buffer.dropRight(1) :+ Drag(start1, current2, area1)
          case (
              Some(Selection(start1, current1, area1)),
              Selection(start2, current2, area2)
              ) if area2.id == area1.id && start1 == (start2) =>
            buffer = buffer.dropRight(1) :+ Selection(start1, current2, area1)
          case _ =>
            buffer = buffer :+ ev

        }
      } else buffer = buffer :+ ev
  }

  def render[K <: Renderable[K]](
      build0: Build[K],
      width: Int = 1000,
      height: Int = 1000,
      hover: Option[(Descriptor, MouseEvent) => Unit] = None,
      unhover: Option[(Descriptor, MouseEvent) => Unit] = None,
      click: Option[(Descriptor, MouseEvent) => Unit] = None,
      selection: Option[collection.Seq[Descriptor] => Unit] = None,
      enableScroll: Boolean = true,
      enableDrag: Boolean = true,
      scaleUpToCanvas: Boolean = true,
      scaleFactor: Double = 1.0
  )(
      implicit er: Renderer[K, CanvasRC]
  ): (html.Canvas, (Build[K], Boolean) => Unit) = {

    var build = build0
    var paintableElem = build.build
    var buildEvents = new BuildEventStore(Vector.empty)

    val width1 =
      if (width <= 0) (paintableElem.bounds.w * scaleFactor).toInt else width
    val height1 =
      if (height <= 0) (paintableElem.bounds.h * scaleFactor).toInt else height

    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style.width = s"${width1}px"
    canvas.style.height = s"${height1}px"
    val devicePixelRatio = window.devicePixelRatio
    canvas.width = (width1 * devicePixelRatio).toInt
    canvas.height = (height1 * devicePixelRatio).toInt

    val ctx =
      CanvasRC(
        canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D],
        hover,
        unhover,
        click,
        selection
      )
    var dragStart = Point(0, 0)
    var dragShift = false
    var animationFrameRequestId: Long = 0

    def paintBounds = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w

      val canvasAspect = canvas.height.toDouble / canvas.width.toDouble

      val paintWidth =
        if (aspect > canvasAspect) canvas.height / aspect else canvas.width
      val paintHeight =
        if (aspect <= canvasAspect) canvas.width * aspect else canvas.height

      if (scaleUpToCanvas || paintableElem.bounds.w * scaleFactor > canvas.width || paintableElem.bounds.h * scaleFactor > canvas.height)
        Bounds(0, 0, paintWidth, paintHeight)
      else
        Bounds(
          0,
          0,
          paintableElem.bounds.w * scaleFactor,
          paintableElem.bounds.h * scaleFactor
        )
    }

    // execution of body may or may not happen
    // if body is important (state change) then use requestAnimationFrame directly
    def queueAnimationFrame(body: Double => Unit) = {

      val old = animationFrameRequestId
      animationFrameRequestId = dom.window.requestAnimationFrame { d =>
        body(d)
      }
      dom.window.cancelAnimationFrame(old.asInstanceOf[Int])

    }

    def paint() = {

      ctx.clear()

      ctx.render(
        fitToBounds(paintableElem, paintBounds)
      )
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        ctx.callIfPlotAreaIsHit(e, p) { descriptor =>
          ctx.mousedown = true
          dragStart = p
          dragShift = e.shiftKey
          click.foreach(cb => cb(descriptor, e))

        }
      }
    }

    def onmouseup(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        if (dragShift) {
          ctx.callSelectionCallback(e, dragStart, p)
        }

      }
    }
    def onclick(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        dom.window.requestAnimationFrame { _ =>
          val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
          ctx.callClickCallbackOnHit(e, p)
        }
      }
    }

    def onmove(e: MouseEvent) = {
      if (e.buttons == 0 && !ctx.mousedown && hover.isDefined) {
        queueAnimationFrame { _ =>
          val p =
            getCanvasCoordinate(canvas, e, devicePixelRatio)
          ctx.callHoverCallbackOnHit(e, p)
        }
      }
      if (e.button == 0 && ctx.mousedown && (selection.isDefined || enableDrag)) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
          val v = Point(dragStart.x - p.x, dragStart.y - p.y)
          val l = math.sqrt(v.x * v.x + v.y * v.y)
          if (l > 0) {
            ctx.callIfPlotAreaIsHit(e, p) { plotAreaDescriptor =>

              val event =
                if (dragShift && selection.isDefined)
                  Some(Selection(dragStart, p, plotAreaDescriptor))
                else if (enableDrag)
                  Some(Drag(dragStart, p, plotAreaDescriptor))
                else None

              if (!dragShift) {
                dragStart = p

              }
              event match {
                case None => ()
                case Some(event) =>
                  paintableElem = build(Some(paintableElem) , event, false)
                  paint()
                  buildEvents.add(event, plotAreaDescriptor.canFuseEvents)

              }

            }

          }
        }
      }
    }

    def onwheel(e: MouseEvent) = {
      e.preventDefault()
      queueAnimationFrame { _ =>
        val cb = canvas.getBoundingClientRect()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        ctx.callIfPlotAreaIsHit(e, p) { plotAreaDescriptor =>
          val event = Scroll(
            e.asInstanceOf[scala.scalajs.js.Dynamic]
              .deltaY
              .asInstanceOf[Double],
            p,
            plotAreaDescriptor
          )
          paintableElem = build(
            Some(paintableElem) , event, false
          )
          paint()
          buildEvents.add(event,plotAreaDescriptor.canFuseEvents)
        }
      }
    }

    val update = { (k: Build[K], clearEvents: Boolean) =>
      dom.window.requestAnimationFrame { _ =>
        build = k
        paintableElem = build.build
        if (clearEvents) {
          buildEvents.clear

        }
        val events = buildEvents.get
        val lastPerId = scala.collection.mutable.AnyRefMap[AnyRef,Event]()
        events.foreach{event =>
          lastPerId.update(event.plotAreaId,event)  
        }
        val lasts = lastPerId.values.toArray

        buildEvents.get.foreach { event =>
          val replayFlag = if (lasts.exists(e => e eq event )) false else true 
          paintableElem = build((Some(paintableElem) , event,replayFlag))
        }
        paint()
      }
      ()
    }

    if (click.isDefined) {
      canvas.onclick = onclick _
    }
    if (selection.isDefined || enableDrag) {
      canvas.onmousedown = onmousedown _

      canvas.onmouseup = { e =>
        ctx.mousedown = false
        onmouseup(e)
      }
    }

    if (selection.isDefined || enableDrag || hover.isDefined) {
      canvas.onmousemove = onmove _
    }

    if (enableScroll) {
      canvas.addEventListener("wheel", onwheel _)
    }

    dom.window.requestAnimationFrame { _ =>
      paint()
    }

    (canvas, update)

  }

  private[nspl] def fill(sh: Shape, graphics: CanvasRenderingContext2D) =
    sh match {
      case sh: Rectangle => {

        graphics.fillRect(sh.x, sh.y, sh.w, sh.h)

      }
      case sh: Ellipse => {
        val centerX = sh.x + 0.5 * sh.w
        val centerY = sh.y + 0.5 * sh.h
        val radiusX = sh.w * 0.5
        val radiusY = sh.h * 0.5

        graphics.beginPath()
        graphics
          .asInstanceOf[scala.scalajs.js.Dynamic]
          .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
        graphics.fill()
      }
      case sh: Line => ()
      case sh: SimplePath =>
        graphics.beginPath()
        sh.ps.foreach { p =>
          graphics.lineTo(p.x, p.y)
        }
        graphics.fill()

      case sh: Path =>
        graphics.beginPath()
        sh.path foreach {
          case cm: MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: CubicTo =>
            graphics.bezierCurveTo(
              cm.p1.x,
              cm.p1.y,
              cm.p2.x,
              cm.p2.y,
              cm.p3.x,
              cm.p3.y
            )
        }
        graphics.fill()

    }

  private[nspl] def draw(
      sh: Shape,
      graphics: CanvasRenderingContext2D,
      stroke: Stroke
  ) = {
    sh match {
      case sh: Rectangle => {
        graphics.strokeRect(sh.x, sh.y, sh.w, sh.h)
      }
      case sh: Ellipse => {
        val centerX = sh.x + 0.5 * sh.w
        val centerY = sh.y + 0.5 * sh.h
        val radiusX = sh.w * 0.5
        val radiusY = sh.h * 0.5

        graphics.beginPath()
        graphics
          .asInstanceOf[scala.scalajs.js.Dynamic]
          .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
        graphics.stroke()
      }
      case sh: Line => {
        graphics.beginPath()
        graphics.moveTo(sh.x1, sh.y1)
        graphics.lineTo(sh.x2, sh.y2)
        graphics.stroke()
      }
      case sh: SimplePath => {
        graphics.beginPath()
        sh.ps.foreach { p =>
          graphics.lineTo(p.x, p.y)
        }
        graphics.stroke()
      }
      case sh: Path => {
        graphics.beginPath()
        sh.path foreach {
          case cm: MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: CubicTo =>
            graphics.bezierCurveTo(
              cm.p1.x,
              cm.p1.y,
              cm.p2.x,
              cm.p2.y,
              cm.p3.x,
              cm.p3.y
            )
        }
        graphics.stroke()
      }
    }
  }

  implicit val shapeRenderer = new Renderer[ShapeElem, CanvasRC] {

    private def drawAndFill(ctx: CanvasRC, elem: ShapeElem) = {

      if (elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)) {
        ctx.setTransformInGraphics()

        val shape = elem.shape

        if (elem.fill.a > 0.0) {
          ctx.withFill(elem.fill) {
            fill(shape, ctx.graphics)
          }
        }
        if (elem.stroke.isDefined && elem.strokeColor.a > 0 && elem.stroke.get.width.factor > 0) {
          ctx.withStroke(elem.strokeColor) {

            ctx.withDash(elem.stroke.get.dashArray) {

              if (ctx.graphics.lineWidth != elem.stroke.get.width.value) {
                ctx.graphics.lineWidth = elem.stroke.get.width.value
              }

              draw(shape, ctx.graphics, elem.stroke.get)
            }
          }
        }
      }
    }

    def render(ctx: CanvasRC, elem: ShapeElem): Unit = {
      ctx.withTransform(elem.tx concat elem.shape.currentTransform) {

        drawAndFill(ctx, elem)

        val descr = elem.descriptor

        descr match {
          case NoDescriptor =>
          case descr: PlotAreaDescriptor =>
            ctx.registerPlotArea(
              elem.shape.resetTransform(_ => ctx.getAffineTransform),
              descr.copy(bounds = Some(elem.bounds))
            )
          case descr =>
            ctx.registerOnClick(
              elem.shape.resetTransform(_ => ctx.getAffineTransform),
              descr
            )
            ctx.registerOnHover(
              elem.shape.resetTransform(_ => ctx.getAffineTransform),
              descr
            )
            ctx.registerOnSelection(
              elem.shape.resetTransform(_ => ctx.getAffineTransform),
              descr
            )
        }

      }

    }
  }

  def asCss(c: Color) = s"rgba(${c.r},${c.g},${c.b}, ${c.a})"

  implicit val textRenderer = new Renderer[TextBox, CanvasRC] {

    def render(ctx: CanvasRC, elem: TextBox): Unit = {
      ctx.withTransform(elem.txLoc) {

        if (!elem.layout.isEmpty) {
          ctx.withFill(elem.color) {
            ctx.graphics.font = canvasFont(elem.font)
            elem.layout.lines.foreach {
              case (line, lineTx) =>
                ctx.withTransform(lineTx) {
                  ctx.setTransformInGraphics()
                  // ctx.graphics.strokeStyle = asCss(elem.color)
                  // ctx.graphics.strokeRect(0, 0, elem.bounds.w, -elem.bounds.h)
                  ctx.graphics.fillText(line, 0, 0)
                }
            }

            val descr = elem.descriptor

            descr match {
              case NoDescriptor =>
              case descr if elem.color.a > 0 && !elem.layout.isEmpty =>
                val boundingBox = elem.bounds
                val sh = Shape
                  .rectangle(
                    boundingBox.x,
                    boundingBox.y,
                    boundingBox.w,
                    boundingBox.h
                  )
                  .transform(_ => ctx.getAffineTransform)
                ctx.registerOnClick(sh, descr)
                ctx.registerOnHover(sh, descr)
                ctx.registerOnSelection(sh, descr)
              case _ =>
            }
          }
        }
      }
    }
  }
}
