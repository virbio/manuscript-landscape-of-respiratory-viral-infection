package stories
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.saddle._
import org.saddle.order._
import org.saddle.circe._
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import _root_.io.circe.Decoder
import org.nspl._
import org.nspl.canvasrenderer.{render => renderToCanvas, _}
import org.nspl.saddle._
import org.saddle.index.InnerJoin
import scala.scalajs.js.ArrayOpsCommon
import scala.language.dynamics
import scala.scalajs.js.typedarray.Uint8Array

import org.portablescala.reflect.annotation.EnableReflectiveInstantiation
import org.portablescala.reflect._
import com.raquo.laminar.tags.HtmlTag
import org.saddle.scalar.ScalarTagDouble

object TestComponent extends CustomComponent {
  type A = String
  def deserialize(s: String): A = s
  def mount(root: org.scalajs.dom.html.Element, data: A): Unit =
    root.textContent = data
}
@EnableReflectiveInstantiation
trait CustomComponent {
  type A
  def deserialize(s: String): A

  def mount(root: dom.html.Element, data: A): Unit
}
@JSExportTopLevel("Stories")
object UI {

  @JSExport
  def mountCustomComponent(
      data: String,
      fqcn: String,
      root: dom.html.Element
  ): Unit = {
    val clsOpt =
      Reflect.lookupLoadableModuleClass(s"${fqcn.stripSuffix("$")}$$")
    clsOpt match {
      case None =>
        throw new RuntimeException(
          s"Failed to instantiate ${fqcn.stripSuffix("$")}$$. The fqcn argument must be the fully qualified class name of an object."
        )
      case Some(cls) =>
        val cls2 = cls.loadModule().asInstanceOf[CustomComponent]
        cls2.mount(root, cls2.deserialize(data))
    }
  }

  def downloadData(data: Array[Byte], filename: String) = {

    def fromBytes(data: Array[Byte]) = {
      import scala.scalajs.js.JSConverters._
      val array = new Uint8Array(data.toJSArray.map(_.toShort))
      new dom.raw.Blob(
        scala.scalajs.js.Array(array),
        org.scalajs.dom.raw.BlobPropertyBag("application/octet-stream")
      )
    }

    if (data.length > 0) {

      import scala.scalajs.js.JSConverters._

      val a = org.scalajs.dom.document
        .createElement("a")
        .asInstanceOf[scala.scalajs.js.Dynamic]

      val url = org.scalajs.dom.URL.createObjectURL(fromBytes(data))

      a.href = url;
      a.download = filename;

      org.scalajs.dom.document.body
        .appendChild(a.asInstanceOf[org.scalajs.dom.raw.Node]);
      a.click();
      scala.scalajs.js.timers.setTimeout(0) {
        org.scalajs.dom.document.body
          .removeChild(a.asInstanceOf[org.scalajs.dom.raw.Node])
        org.scalajs.dom.URL.revokeObjectURL(url)
      }
    }
  }

  @JSExport
  def mountSequenceViewerSerialized(
      root: dom.html.Element,
      sequencesSerialized: String
  ) = {

    val sequences =
      _root_.io.circe.parser
        .decode[Seq[(String, String)]](sequencesSerialized)
        .right
        .get

    mountSequenceViewer(root, sequences)
  }
  @JSExport
  def mountSequenceViewer(
      root: dom.html.Element,
      sequences: Seq[(String, String)]
  ) = {

    // val alphabet = sequences.map(_._2.toSet).foldLeft(Set.empty)(_++_)
    def backgroundOf(c0: Char) = {
      val c = c0.toLower
      val color =
        if ("ed".contains(c)) colorPick(1, 8)
        else if ("pg".contains(c)) colorPick(2, 8)
        else if ("avmli".contains(c)) colorPick(3, 8)
        else if ("kr".contains(c)) colorPick(4, 8)
        else if ("ntqs".contains(c)) colorPick(5, 8)
        else if ("fyw".contains(c)) colorPick(6, 8)
        else if ("c".contains(c)) colorPick(7, 8)
        else Color.white
      s"rgba(${color.r},${color.g},${color.b})"
    }

    val maxLength = sequences.map(_._2.length()).max

    val appDiv: Div =
      div(
        fontFamily := "monospace",
        // fontSize := "larger",
        whiteSpace := "pre",
        lineHeight := "1",
        display := "flex",
        div(
          p(span(" "), marginTop := "0", marginBottom := "0"),
          p(span(" "), marginTop := "0", marginBottom := "0"),
          sequences.map {
            case (name, _) =>
              p(span(name), marginTop := "0", marginBottom := "0")
          },
          paddingRight := "10px"
        ),
        div(
          overflowX := "scroll", {
            var cum = 1
            p(
              (1 to maxLength)
                .map {
                  case idx =>
                    if (idx % 10 == 0) {
                      val text = ((idx / 10) * 10).toString
                      cum += text.length
                      span(
                        text
                      )
                    } else if (cum % 10 != 0 || idx < 10) {
                      cum += 1
                      span(" ")
                    } else span("")

                },
              marginBottom := "0"
            )
          }, {
            p(
              (1 to maxLength)
                .map {
                  case (idx) =>
                    if (idx % 10 == 0) span(" ")
                    else
                      span(
                        (idx % 10).toString
                      )

                },
              marginBottom := "0",
              marginTop := "0"
            )
          },
          sequences.toSeq
            .map {
              case (name, seq) =>
                p(
                  seq.zipWithIndex
                    .map {
                      case (c, idx) =>
                        span(
                          c.toString,
                          backgroundColor := backgroundOf(
                            c
                          )
                        )
                    },
                  marginTop := "0",
                  marginBottom := "0"
                )
            }
        )
      )

    render(root, appDiv)

  }

  @JSExport
  def mountTableOfContents(
      root: dom.html.Element,
      titles: scala.scalajs.js.Array[String],
      refs: scala.scalajs.js.Array[String],
      sections: scala.scalajs.js.Array[Boolean]
  ) = {
    case class State(
        text: String
    ) {
      def update(c: Command): State = c match {
        case SetText(p) => copy(text = p)
        case _          => this
      }
    }

    sealed trait Command

    case class SetText(p: String) extends Command

    val state = Var(
      State(
        ""
      )
    )

    val commandObserver =
      Observer[Command](command => state.update(_.update(command)))

    val zipped = titles.toList zip refs.toList zip sections.toList

    val nav =
      new HtmlTag[dom.html.Element]("nav")

    val appDiv: Div = div(
      cls := "overflow-y-scroll hide-lg position-sticky top-8 ",
      maxHeight := "calc(100vh - 0rem)",
      width := "300px",
      div(
        display := "unset",
        div(
          cls := "subnav-search mb-2",
          input(
            cls("form-control"),
            paddingLeft := "30px",
            typ := "search",
            inContext(
              thisNode =>
                onInput
                  .mapTo(thisNode.ref.value)
                  .map(SetText(_))
                  --> commandObserver
            )
          ),
          svg.svg(
            svg.cls := "subnav-search-icon octicon octicon-search",
            svg.width := "16",
            svg.height := "16",
            svg.viewBox := "0 0 16 16",
            svg.path(
              svg.fillRule := "evenodd",
              svg.d := "M15.7 13.3l-3.81-3.83A5.93 5.93 0 0 0 13 6c0-3.31-2.69-6-6-6S1 2.69 1 6s2.69 6 6 6c1.3 0 2.48-.41 3.47-1.11l3.83 3.81c.19.2.45.3.7.3.25 0 .52-.09.7-.3a.996.996 0 0 0 0-1.41v.01zM7 10.7c-2.59 0-4.7-2.11-4.7-4.7 0-2.59 2.11-4.7 4.7-4.7 2.59 0 4.7 2.11 4.7 4.7 0 2.59-2.11 4.7-4.7 4.7z"
            )
          )
        ),
        nav(
          cls := " SideNav border",
          children <-- state.signal.map(_.text).distinct.map { search =>
            val filtered =
              if (search.isEmpty) zipped
              else
                zipped
                  .filter(_._1._1.toLowerCase.contains(search.toLowerCase()))
            filtered.map {
              case ((title, ref), section) =>
                a(
                  cls := "SideNav-item",
                  href := ref,
                  if (!section) title
                  else h5(title)
                )
            }
          }
        )
      )
    )
    render(root, appDiv)

  }

  @JSExport
  def mountTabNavigation(
      root: dom.html.Element,
      titles: scala.scalajs.js.Array[String],
      children: scala.scalajs.js.Array[dom.html.Element]
  ): Unit = {
    render(root, tabNavigation(titles.toSeq zip children.toSeq.map { root =>
      val d = div()
      d.ref.appendChild(root)
      root.style = ""
      d
    }))
  }

  def tabNavigation(
      children: Seq[
        (String, ReactiveHtmlElement[org.scalajs.dom.html.Element])
      ],
      logo: Option[ReactiveHtmlElement[org.scalajs.dom.html.Element]] = None
  ) = {

    case class State(
        page: Int
    ) {
      def update(c: Command): State = c match {
        case SetPage(p) => copy(page = p)
        case _          => this
      }
    }

    sealed trait Command

    case class SetPage(p: Int) extends Command

    case class PageData(
        title: String,
        root: ReactiveHtmlElement[org.scalajs.dom.html.Element],
        handle: Int
    )

    val state = Var(
      State(
        0
      )
    )

    val commandObserver =
      Observer[Command](command => state.update(_.update(command)))

    val ariacurrent = htmlProp(
      "aria-current",
      com.raquo.laminar.codecs.BooleanAsAttrPresenceCodec
    )

    val nav =
      new HtmlTag[dom.html.Element]("nav")

    def pager(pages: Seq[PageData]) = {
      div(
        if (pages.size <= 1) div()
        else
          div(
            cls := "tabnav position-sticky top-0 bg-white",
            height := "37px",
            width := "100%",
            zIndex := "100",
            nav(
              cls := "tabnav-tabs",
              logo.map { element =>
                element.amend(height := "37px")
              }.toList ++
                pages.map { page =>
                  button(
                    `type` := "button",
                    cls := "tabnav-tab btn btn-invisible",
                    aria.ariaAttr(
                      "current",
                      com.raquo.laminar.codecs.BooleanAsAttrPresenceCodec
                    ) <-- state.signal
                      .map(_.page)
                      .distinct
                      .map(
                        pageIdx => if (pageIdx == page.handle) true else false
                      ),
                    page.title,
                    onClick.map(_ => SetPage(page.handle)) --> commandObserver
                  )
                }
            )
          ),
        div(
          child <-- state.signal.map(_.page).distinct.map { page =>
            pages.find(_.handle == page).map(_.root).getOrElse(div())
          }
        )
      )
    }

    val pages =
      children.zipWithIndex.map {
        case ((title, contentRoot), idx) =>
          // contentRoot.ref.style = ""
          PageData(title, contentRoot, idx)
      }

    val appDiv: Div = div(
      pager(pages)
    )

    appDiv

  }

  def mount3DGraph(
      root: dom.html.Element,
      nodeFrame: Frame[String, Int, Double],
      nodeClusters: Vec[Int],
      edgesMat: Mat[Double],
      width: Int,
      legend: Seq[(Int, String)],
      extraTextMap: Map[Int, String],
      edgeColors: Seq[(Double, Color)]
  ): Unit = {
    val layout = nodeFrame.resetColIndex

    val lineSegmentPositions =
      Mat(
        edgesMat.rows
          .map { row =>
            val v1 = row.raw(0).toInt
            val v2 = row.raw(1).toInt
            val v1Coord = layout.rowAt(v1).toVec
            val v2Coord = layout.rowAt(v2).toVec
            Vec(
              v1Coord.raw(0),
              v1Coord.raw(1),
              v1Coord.raw(2),
              v2Coord.raw(0),
              v2Coord.raw(1),
              v2Coord.raw(2),
              row.raw(2)
            )

          }: _*
      ).T

    val numClusters = nodeClusters.toSeq.distinct.size

    val (cameraTarget, cameraPosition, maxExtent) = {
      val xMin = nodeFrame.colAt(0).toVec.head(100).min.get
      val xMax = nodeFrame.colAt(0).toVec.head(100).max.get
      val yMin = nodeFrame.colAt(1).toVec.head(100).min.get
      val yMax = nodeFrame.colAt(1).toVec.head(100).max.get
      val zMin = nodeFrame.colAt(2).toVec.head(100).min.get
      val zMax = nodeFrame.colAt(2).toVec.head(100).max.get

      val cameraTarget = Math3D.Vec3(
        (xMin + 0.5 * (xMax - xMin)).toFloat,
        (yMin + 0.5 * (yMax - yMin)).toFloat,
        zMin.toFloat
      )
      val width = (xMax - xMin)
      val cameraPosition = Math3D.Vec3(
        (xMin + 0.5 * (xMax - xMin)).toFloat,
        (yMin + 0.5 * (yMax - yMin)).toFloat,
        (zMin + width).toFloat
      )
      (
        cameraTarget,
        cameraPosition,
        scala.math.max(zMax - zMin, scala.math.max(xMax - xMin, yMax - yMin))
      )
    }
    val edgeColorMap =
      if (edgeColors.nonEmpty) ManualColor(edgeColors.toMap)
      else GrayScale(min = 0d, max = 1d, white = 250)

    def makePlot() = {

      xyzplot(
        lineSegmentPositions -> lineSegment3D(
          color = edgeColorMap,
          stroke = Stroke(lineWidth * 2)
        ),
        layout.addCol(
          Series(nodeClusters.map(_.toDouble), layout.rowIx),
          2,
          InnerJoin
        ) -> point3D(
          size = (0.04 fts).value,
          color = DiscreteColors(nodeClusters.max.get)
        )
      )(
        cameraTarget = cameraTarget,
        cameraPosition = cameraPosition,
        dragFactor = maxExtent / 100,
        scrollFactor = maxExtent / 100000,
        extraLegend = legend.toSeq
          .sortBy(_._1)
          .map {
            case (cl, l) =>
              (
                l,
                PointLegend(
                  org.nspl.shapePick(cl),
                  DiscreteColors(numClusters)(cl)
                )
              )
          },
        legendLayout =
          TableLayout(4, horizontalGap = 0 fts, verticalGap = 0 fts),
        legendToPlotLayout = VerticalStack(Anchor, 0.5 fts),
        legendFontSize = 0.15 fts
      )
    }

    case class State(
        clicked: Option[Int] = None
    ) {

      def update(c: Command) = c match {

        case Clicked(idx) => copy(clicked = Some(idx))

        case _ =>
          println(c)
          this
      }
    }

    var state = Var(State())

    sealed trait Command
    case class SetFilter(s: String) extends Command
    case class Clicked(idx: Int) extends Command
    val commandObserver = Observer[Command] { c =>
      state.update(_.update(c))
    }

    val (plotHolder, updater) =
      org.nspl.canvasrenderer.render(makePlot(), width, click = Some {
        case (DataRowIdx(_, dataSourceIdx, rowIdx),_) if dataSourceIdx == 1 =>
          commandObserver.onNext(Clicked(rowIdx))
        case _ =>
      })

    val container = dom.document.createElement("div")
    container.setAttribute("class", "width-fit border bg-white")
    container.appendChild(plotHolder)

    val form = div(
      // input(
      //   cls("form-control input-sm m-1"),
      //   typ := "text",
      //   placeholder := "node filter (regex)",
      //   inContext(
      //     thisNode =>
      //       onChange
      //         .mapTo(thisNode.ref.value)
      //         .map(SetFilter(_))
      //         --> commandObserver
      //   )
      // ),
      div(
        display := "inline",
        child <-- state.signal.map(_.clicked).distinct.map {
          case Some(clickedIdx) =>
            val text: String = extraTextMap.get(clickedIdx).getOrElse("")
            span(text)
          case None => span()
        }
      )
    )

    render(root, form)

    root.appendChild(container)
  }

  @JSExport
  def mountInteractiveGraphPlot(
      root: dom.html.Element,
      nodeLocations: String,
      edges: String,
      nodeClusters: String,
      legendText: String,
      extraText: String,
      edgeColorText: String,
      width: Int
  ) = {

    val nodeFrame =
      _root_.io.circe.parser
        .decode[Frame[String, Int, Double]](nodeLocations)
        .right
        .get
    val edgesMat =
      _root_.io.circe.parser
        .decode[Mat[Double]](edges)
        .right
        .get
    val clusters = _root_.io.circe.parser
      .decode[Vec[Int]](nodeClusters)
      .right
      .get
    val legend = _root_.io.circe.parser
      .decode[Seq[(Int, String)]](legendText)
      .right
      .get

    val extraTextMap = _root_.io.circe.parser
      .decode[Series[Int, String]](extraText)
      .right
      .get
      .toSeq
      .toMap

    val edgeColors: Seq[(Double, Color)] = _root_.io.circe.parser
      .decode[Seq[(Double, Int, Int, Int, Int)]](edgeColorText)
      .right
      .get
      .toSeq
      .map { case (v, r, g, b, a) => (v, Color(r, g, b, a)) }

    if (nodeFrame.numCols >= 3)
      mount3DGraph(
        root,
        nodeFrame,
        clusters,
        edgesMat,
        width,
        legend,
        extraTextMap,
        edgeColors
      )
    else {

      val numClusters = clusters.toSeq.distinct.size

      val layout = nodeFrame.resetColIndex

      val sections = Mat(
        edgesMat.rows
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

      def makePlot(filter: String) = {
        val width = nodeFrame.colAt(0).toVec.max2 - nodeFrame
          .colAt(0)
          .toVec
          .min2
        val height = nodeFrame.colAt(1).toVec.max2 - nodeFrame
          .colAt(1)
          .toVec
          .min2
        val focus = nodeFrame.rfilterIx(ix => ix.matches(filter))
        val xMin = focus.colAt(0).min.getOrElse(Double.NaN) - width * 0.05
        val xMax = focus.colAt(0).max.getOrElse(Double.NaN) + width * 0.05
        val yMin = focus.colAt(1).min.getOrElse(Double.NaN) - height * 0.05
        val yMax = focus.colAt(1).max.getOrElse(Double.NaN) + height * 0.05
        val xLim =
          if (focus.isEmpty) None
          else Some(xMin -> xMax)
        val yLim = if (focus.isEmpty) None else Some(yMin -> yMax)

        xyplot(
          sections -> lineSegment(
            color = GrayScale(min = 0d, max = 1d, white = 250)
              .mapColors(_.copy(a = 130)),
            colorCol = 4,
            strokes = List(Stroke(lineWidth * 0.3))
          ),
          layout.addCol(
            Series(clusters.map(_.toDouble), layout.rowIx),
            2,
            InnerJoin
          ) -> point(
            size = (0.1 fts).value,
            labelText = true,
            labelFontSize = 0.3 fts,
            labelFontBold = true,
            forceDirectedLabelLayout = true,
            maxForceDirectedLabels = 150,
            labelConnectionLineColor = Color(230, 216, 167),
            color = DiscreteColors(numClusters),
            shapeCol = 2,
            noDescriptor = false
          )
        )(
          par(
            xlim = xLim,
            ylim = yLim,
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
            yDropOverhangingTickLabels = true,
            xDropOverhangingTickLabels = true,
            extraLegend = legend.toSeq
              .sortBy(_._1)
              .map {
                case (cl, l) =>
                  (
                    l,
                    PointLegend(
                      org.nspl.shapePick(cl),
                      DiscreteColors(numClusters)(cl)
                    )
                  )
              },
            legendLayout =
              TableLayout(4, horizontalGap = 0 fts, verticalGap = 0 fts),
            legendToPlotLayout = VerticalStack(Anchor, 0.5 fts),
            legendFontSize = 0.15 fts
          )
        )
      }

      case class State(
          filter: String = "",
          clicked: Option[Int] = None
      ) {

        def update(c: Command) = c match {

          case Clicked(idx) => copy(clicked = Some(idx))

          case SetFilter(s) =>
            if (s == filter) this
            else
              copy(
                filter = s
              )

          case _ =>
            println(c)
            this
        }
      }

      var state = Var(State())

      sealed trait Command
      case class SetFilter(s: String) extends Command
      case class Clicked(idx: Int) extends Command
      val commandObserver = Observer[Command] { c =>
        state.update(_.update(c))
      }

      val (plotHolder, updater) =
        org.nspl.canvasrenderer.render(makePlot(""), width, click = Some {
          case (DataRowIdx(_, dataSourceIdx, rowIdx),_) if dataSourceIdx == 1 =>
            commandObserver.onNext(Clicked(rowIdx))
          case _ =>
        })

      val container = dom.document.createElement("div")
      container.setAttribute("class", "width-fit border bg-white")
      container.appendChild(plotHolder)

      implicit val owner = new Owner {}
      state.signal
        .map(_.filter)
        .distinct
        .addObserver(Observer { filter =>
          updater(makePlot(filter),true)
        })

      val form = div(
        input(
          cls("form-control input-sm m-1"),
          typ := "text",
          placeholder := "node filter (regex)",
          inContext(
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value)
                .map(SetFilter(_))
                --> commandObserver
          )
        ),
        div(
          display := "inline",
          child <-- state.signal.map(_.clicked).map {
            case Some(clickedIdx) =>
              val text: String = extraTextMap.get(clickedIdx).getOrElse("")
              span(text)
            case None => span()
          }
        )
      )

      render(root, form)

      root.appendChild(container)
    }
  }

  @JSExport
  def mountInteractiveScatters(
      root: dom.html.Element,
      serializedDescription: String,
      width: Int
  ) = {

    val description =
      _root_.io.circe.parser
        .decode[ScatterDescription](serializedDescription)
        .right
        .get

    case class State(
        selected: Set[DataRowIdx] = Set.empty
    ) {

      def update(c: Command) = c match {

        case SetSelection(s) =>
          copy(
            selected = s
          )

        case _ =>
          println(c)
          this
      }
    }

    var state = Var(State())

    sealed trait Command
    case class SetSelection(s: Set[DataRowIdx]) extends Command
    val commandObserver = Observer[Command] { c =>
      state.update(_.update(c))
    }

    val canvases = description.panels.zipWithIndex.map {
      case (descr, idx) =>
        val (canvas, updater) =
          org.nspl.canvasrenderer.render(
            xyplot(
              descr.data -> point(
                size = 2d,
                color = RedBlue(min = -5, max = 5d, mid = Some(0d), white = 230),
                shapes = Vector(shapeList(1)),
                noDescriptor = false
              )
            )(
              par(
                xNoTickLabel = true,
                yNoTickLabel = true,
                main = descr.main,
                externalDataSourceIdx = Some(idx),
                topPadding = lineWidth,
                bottomPadding = lineWidth,
                leftPadding = lineWidth,
                rightPadding = lineWidth
              )
            ),
            width = (width / (description.columns + 0.2)).toInt,
            height = (width / (description.columns + 0.2)).toInt,
            hover = None,
            selection = Some { selected =>
              commandObserver.onNext(SetSelection(selected.collect {
                case dr: DataRowIdx => dr
              }.toSet))
            }
          )
        canvas
    }

    val container = dom.document.createElement("div")
    container.setAttribute("class", "bg-white")
    container.setAttribute("style", "display:flex; flex-wrap: wrap; gap: 5px;")
    container.append(canvases: _*)

    // implicit val owner = new Owner {}
    // state.signal
    //   .map(_.filter)
    //   .addObserver(Observer { filter =>
    //     updater(makePlot(filter))
    //   })

    val selectionDisplay = div(
      display := "flex",
      flexDirection := "column",
      div(
        // display := "inline",
        child <-- state.signal.map(_.selected).distinct.map { selected =>
          val frame = Frame
            .fromCols(selected.toList.map { rowIdx =>
              val row = description
                .panels(rowIdx.externalDataSourceIdx)
                .data
                .rowIx
                .at(rowIdx.rowIdx)
                .get
              (row, description.rowDescriptions.first(row))

            }: _*)
            .T

          tableViewer(
            frame
              .setRowIndex(frame.rowIx.toSeq.map(List(_)).zipWithIndex.toIndex),
            highlightCutoffLow = None,
            highlightCutoffHigh = None,
            keyHeader = None,
            ignoreRowIx = false,
            overflow = false,
            roundTo = 4,
            caseInsensitive = false,
            hideProjection = false,
            downloadButton = true
          )

        }
      )
    )

    render(root, selectionDisplay)

    root.firstChild.insertBefore(container, root.firstChild.firstChild)

  }

  @JSExport
  def mountStringTable(
      root: dom.html.Element,
      data: String,
      ignoreRowIx: Boolean
  ) = {
    mountTable[String](
      root,
      data,
      ignoreRowIx = ignoreRowIx,
      roundTo = 4
    )
  }
  @JSExport
  def mountDoubleTable(
      root: dom.html.Element,
      data: String,
      ignoreRowIx: Boolean,
      roundTo: Int
  ) = {
    mountTable[Double](
      root = root,
      data = data,
      ignoreRowIx = ignoreRowIx,
      roundTo = roundTo
    )
  }
  @JSExport
  def mountDoubleTableWithHighlights(
      root: dom.html.Element,
      data: String,
      low: Double,
      high: Double,
      ignoreRowIx: Boolean,
      roundTo: Int
  ) = {
    mountTable[Double](
      root= root,
      data = data,
      roundTo = roundTo,
     highlightCutoffLow = Some(low),
     highlightCutoffHigh=  Some(high),
      ignoreRowIx=ignoreRowIx
    )
  }
  def mountTable[A](
      root: dom.html.Element,
      data: String,
      roundTo: Int,
      ignoreRowIx: Boolean,
      highlightCutoffLow: Option[A] = None,
      highlightCutoffHigh: Option[A] = None,
      caseInsensitive: Boolean = false
  )(
      implicit dec: Decoder[Frame[List[String], String, A]],
      ord: Ordering[A],
      st: ST[A]
  ): Unit = {

    val (keyHeader, frame) = {
      val (keyHeader, f1) =
        _root_.io.circe.parser
          .decode[(Option[Seq[String]], Frame[List[String], String, A])](data)
          .right
          .get
      (keyHeader, f1.setRowIndex(f1.rowIx.toSeq.zipWithIndex.toIndex))
    }

    val t = tableViewer(
      frame,
      highlightCutoffLow,
      highlightCutoffHigh,
      keyHeader,
      ignoreRowIx,
      false,
      roundTo,
      caseInsensitive,
      false,
      false
    )
    render(root, t)
  }

  def tableViewer[A](
      frame: Frame[(List[String], Int), String, A],
      highlightCutoffLow: Option[A],
      highlightCutoffHigh: Option[A],
      keyHeader: Option[Seq[String]],
      ignoreRowIx: Boolean,
      overflow: Boolean,
      roundTo: Int,
      caseInsensitive: Boolean,
      hideProjection: Boolean,
      downloadButton: Boolean
  )(
      implicit
      ord: Ordering[A],
      st: ST[A]
  ) = {

    def highlightClass(s: A) =
      if (highlightCutoffLow.exists(th => ord.lt(s, th))) ("bg-red-light")
      else if (highlightCutoffHigh.exists(th => ord.gt(s, th)))
        ("bg-green-light")
      else ("")

    sealed trait Command
    case class SetFilter(s: String) extends Command
    case class SetProjection(s: Seq[String]) extends Command
    case class SetSortOrder(col: SortBy, ascending: Boolean) extends Command
    case class SetHighlight(i: Int) extends Command

    sealed trait SortBy
    case class Col(cix: String) extends SortBy
    case object Initial extends SortBy
    case object RowIx extends SortBy

    case class State(
        projection: Seq[String] = Nil,
        filter: String = "",
        page: Int = 0,
        pageSize: Int = 100,
        sortCol: SortBy = Initial,
        sortAsc: Boolean = true,
        filteredFrame: Frame[(List[String], Int), String, A],
        highlighted: Set[Int] = Set.empty
    ) {

      def sortByIs(s: String) = sortCol match {
        case Col(c) if c == s => true
        case _                => false
      }
      def maxPages = scala.math.ceil(filteredFrame.numRows / pageSize.toDouble)
      def update(c: Command) = c match {
        case SetHighlight(i) if highlighted.contains(i) =>
          copy(highlighted = highlighted - i)
        case SetHighlight(i) => copy(highlighted = highlighted + i)
        case SetSortOrder(col, ascending)
            if (col != sortCol || ascending != sortAsc) =>
          copy(
            sortCol = col,
            sortAsc = ascending,
            filteredFrame = sort(filteredFrame, col, ascending)
          )
        case SetFilter(s) =>
          if (s == filter) this
          else
            copy(
              filter = s,
              filteredFrame = {
                val pf = projectAndFilter(projection, s)
                if (sortCol == Initial && sortAsc) pf
                else sort(pf, sortCol, sortAsc)
              },
              page = 0
            )
        case SetProjection(s) =>
          if (s == projection) this
          else if (s.nonEmpty && projection.containsSlice(s))
            copy(projection = s, filteredFrame = project(filteredFrame, s))
          else
            copy(
              projection = s,
              filteredFrame = {
                val pf = projectAndFilter(s, filter)
                if (sortCol == Initial && sortAsc) pf
                else sort(pf, sortCol, sortAsc)
              }
            )
        case _ =>
          this
      }
    }
    def sort(
        frame: Frame[(List[String], Int), String, A],
        col: SortBy,
        asc: Boolean
    ) = {

      col match {
        case Initial =>
          if (asc) frame.rowAt(frame.rowIx.map(_._2).argSort)
          else frame.rowAt(frame.rowIx.map(_._2).argSort.reverse)
        case RowIx =>
          if (asc) frame.rowAt(frame.rowIx.map(_._1).argSort)
          else frame.rowAt(frame.rowIx.map(_._1).argSort.reverse)
        case Col(col) =>
          if (asc)
            frame.rowAt(
              array.argsort(
                frame
                  .firstCol(col)
                  .toVec
                  .map { a =>
                    val str = Option(st.asString(a)).getOrElse("NA")

                    val d =
                      ScalarTagDouble.parse(str.toCharArray(), 0, str.length())

                    (d, str)
                  }
                  .fillNA(_ => (Double.NaN, "NA"))
              )
            )
          else
            frame.rowAt(
              array
                .argsort(
                  frame
                    .firstCol(col)
                    .toVec
                    .map { a =>
                      val str = Option(st.asString(a)).getOrElse("NA")
                      assert(str != null)
                      val d =
                        ScalarTagDouble
                          .parse(str.toCharArray(), 0, str.length())
                      (d, str)
                    }
                    .fillNA(_ => (Double.NaN, "NA"))
                )
                .reverse
            )
      }
    }
    def project[RX, T](frame: Frame[RX, String, T], projection: Seq[String]) = {
      val projectionsR = projection.map(_.r)
      frame.filterIx(
        cix => projectionsR.exists(r => r.pattern.matcher(cix).matches)
      )
    }
    def projectAndFilter(
        projection: Seq[String],
        filter: String
    ): Frame[(List[String], Int), String, A] = {
      val pattern =
        if (caseInsensitive)
          java.util.regex.Pattern
            .compile(filter, java.util.regex.Pattern.CASE_INSENSITIVE)
        else filter.r.pattern
      (if (projection.isEmpty) frame
       else project(frame, projection))
        .rfilterIx(
          ix =>
            if (filter.isEmpty) true
            else ix._1.exists(str => pattern.matcher(str).matches)
        )

    }
    var state = Var(State(filteredFrame = projectAndFilter(Nil, "")))

    val commandObserver = Observer[Command] { c =>
      state.update(_.update(c))
    }

    def downloadB =
      if (downloadButton)
        span(
          button(
            typ := "button",
            cls := "btn-octicon",
            svg.svg(
              svg.viewBox := "0 0 16 16",
              svg.width := "16",
              svg.height := "16",
              svg.path(
                svg.fillRule := "evenodd",
                svg.d := "M7.47 10.78a.75.75 0 001.06 0l3.75-3.75a.75.75 0 00-1.06-1.06L8.75 8.44V1.75a.75.75 0 00-1.5 0v6.69L4.78 5.97a.75.75 0 00-1.06 1.06l3.75 3.75zM3.75 13a.75.75 0 000 1.5h8.5a.75.75 0 000-1.5h-8.5z"
              )
            ),
            onClick --> { _ =>
              val downloadName = "data.csv"
              keyHeader match {
                case None =>
                  downloadData(
                    org.saddle.csv.CsvWriter
                      .writeFrameToArray(state.now.filteredFrame),
                    downloadName
                  )
                case Some(value) =>
                  val f = state.now.filteredFrame
                  val expandedRowIxIntoFrame = value.zipWithIndex.map {
                    case (name, idx) =>
                      val ix = f.rowIx
                      name -> Series(ix, ix.toVec.map(_._1.apply(idx)))

                  }.toFrame
                  val frameAsString = f.mapValues(_.toString)
                  val f2 = (expandedRowIxIntoFrame rconcat frameAsString).resetRowIndex
                  downloadData(
                    org.saddle.csv.CsvWriter
                      .writeFrameToArray(f2),
                    downloadName
                  )

              }

            }
          )
        )
      else span()
    def pager =
      span(
        button(
          disabled <-- state.signal.map(_.page <= 0).distinct,
          hidden <-- state.signal.map(_.page <= 0).distinct,
          typ := "button",
          cls := "btn-octicon",
          svg.svg(
            svg.viewBox := "0 0 6 16",
            svg.width := "16",
            svg.height := "16",
            svg.path(svg.fillRule := "evenodd", svg.d := "M6 2L0 8l6 6V2z")
          ),
          inContext(
            thisNode =>
              onClick
                .mapTo(thisNode.ref.value)
                --> Observer[String](
                  _ =>
                    state.update(
                      s => s.copy(page = scala.math.max(0, s.page - 1))
                    )
                )
          )
        ),
        span(
          cls("f5 text-gray-light"),
          child.text <-- state.signal
            .map(s => s"${s.page + 1}/${s.maxPages}")
            .distinct
        ),
        button(
          disabled <-- state.signal
            .map(st => st.page + 1 >= st.maxPages)
            .distinct,
          hidden <-- state.signal
            .map(st => st.page + 1 >= st.maxPages)
            .distinct,
          typ := "button",
          cls := "btn-octicon",
          svg.svg(
            svg.viewBox := "0 0 6 16",
            svg.width := "16",
            svg.height := "16",
            svg.path(svg.fillRule := "evenodd", svg.d := "M0 14l6-6-6-6v12z")
          ),
          inContext(
            thisNode =>
              onClick
                .mapTo(thisNode.ref.value)
                --> Observer[String](
                  _ => state.update(s => s.copy(page = s.page + 1))
                )
          )
        )
      )
    val tab = div(
      div(
        cls := "width-fit d-flex flex-row flex-justify-end flex-items-center",
        span(
          cls("f5 text-gray-light"),
          "Dimensions: " + frame.numRows + " x " + frame.numCols
        ),
        downloadB,
        span(cls("f5 text-gray-light ml-3"), "Sort by:"),
        select(
          cls("form-select select-sm m-1"),
          typ := "select",
          inContext(
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value)
                .map { s =>
                  val asc = s.contains("ASC")
                  if (s.contains("Row header")) SetSortOrder(RowIx, asc)
                  else if (s.contains("Default")) SetSortOrder(Initial, asc)
                  else
                    SetSortOrder(
                      Col(s.split(' ').dropRight(1).mkString(" ")),
                      asc
                    )
                }
                --> commandObserver
          ),
          children <-- state.signal
            .map(
              st =>
                option(
                  "Row header ASC",
                  selected := (st.sortCol == RowIx && st.sortAsc)
                ) ::
                  option(
                    "Row header DESC",
                    selected := st.sortCol == RowIx && !st.sortAsc
                  ) ::
                  option(
                    "Default order ASC",
                    selected := st.sortCol == Initial && st.sortAsc
                  ) ::
                  option(
                    "Default order DESC",
                    selected := st.sortCol == Initial && !st.sortAsc
                  ) ::
                  st.filteredFrame.colIx.toSeq.toList
                    .flatMap(
                      s =>
                        List(
                          option(
                            s + " ASC",
                            selected := st.sortByIs(s) && st.sortAsc
                          ),
                          option(
                            s + " DESC",
                            selected := st.sortByIs(s) && !st.sortAsc
                          )
                        )
                    )
            )
            .distinct
        ),
        input(
          cls("form-control input-sm m-1"),
          typ := "text",
          width := (if (caseInsensitive) "250px" else "auto"),
          placeholder := s"filter (${if (caseInsensitive) "case insensitive "
          else ""}regex)",
          inContext(
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value)
                .map(SetFilter(_))
                --> commandObserver
          )
        ),
        if (hideProjection) span()
        else
          input(
            cls("form-control  input-sm m-1"),
            typ := "text",
            placeholder := "projection (space delimited)",
            width := "200px",
            inContext(
              thisNode =>
                onChange
                  .mapTo(thisNode.ref.value)
                  .map(
                    s => SetProjection(s.trim.split("\\s+").filter(_.nonEmpty))
                  )
                  --> commandObserver
            )
          ),
        pager
      ),
      div(
        if (overflow) cls("overflow-x-auto") else display := "unset",
        table(
          thead(
            child <-- state.signal.map { state =>
              val firstRowHeaderSize = state.filteredFrame.toRowSeq.headOption
                .map(_._1._1.size)
                .getOrElse(1)
              if (ignoreRowIx)
                tr(
                  th(
                    "Row number"
                  ) +:
                    state.filteredFrame.colIx.toSeq
                      .map(
                        s =>
                          th(
                            s
                          )
                      )
                )
              else
                tr(
                  th(
                    "Row number"
                  ) +:
                    (keyHeader
                      .map(_.map(th(_)))
                      .getOrElse(
                        0 until firstRowHeaderSize map (
                            i =>
                              th(
                                s"Row key ${if (firstRowHeaderSize > 1) (i + 1).toString
                                else ""}"
                              )
                          )
                      ) ++

                    state.filteredFrame.colIx.toSeq
                      .map(
                        s =>
                          th(
                            s
                          )
                      ))
                )
            }.distinct
          ),
          tbody(
            children <-- state.signal
              .map(
                st => (st.filteredFrame, st.page, st.pageSize, st.highlighted)
              )
              .distinct
              .map {
                case (frame, page, pageSize, highlighted) =>
                  frame
                    .rowAt(
                      (
                        page * pageSize,
                        (page + 1) * pageSize
                      )
                    )
                    .toRowSeq
                    .toList
                    .map {
                      case ((rowIx, idx), values) =>
                        tr(
                          cls(
                            if (highlighted.contains(idx)) "bg-blue-light"
                            else ""
                          ),
                          inContext(
                            thisNode =>
                              onClick
                                .map(
                                  s => SetHighlight(idx)
                                )
                                --> commandObserver
                          ), {

                            val valueFields = values.toVec.toSeq
                              .map(
                                s =>
                                  td(
                                    cls(
                                      if (highlighted.contains(idx)) ""
                                      else
                                        highlightClass(
                                          s
                                        )
                                    ),
                                    s match {
                                      case d: Double =>
                                        s"%.${roundTo}f".format(d)
                                      case x => x.toString
                                    }
                                  )
                              )
                            if (ignoreRowIx)
                              td((idx + 1).toString) +: valueFields
                            else
                              td((idx + 1).toString) +:
                                (rowIx.map(td(_)) ++ valueFields)

                          }
                        )
                    }
              }
          )
        )
      ),
      div(
        cls := "width-fit d-flex flex-row flex-justify-end flex-items-center",
        pager
      )
    )

    tab

  }
}
