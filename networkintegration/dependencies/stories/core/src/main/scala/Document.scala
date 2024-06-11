package stories
import org.saddle.{Frame, ST, NUM, Mat, Vec, Series}
import org.saddle.scalar.ScalarTagDouble
import org.saddle.order._
import org.saddle.csv.CsvSettings
case class Table(
    tableHeader: Seq[String],
    tableRows: Seq[Seq[String]]
) {
  def render = {
    s"""<div>
        <table>
  <thead>
    <tr>
     ${tableHeader.map(s => s"""<th>$s</th>""").mkString}
    </tr>
  </thead>
  <tbody>
    ${tableRows
      .map(
        row =>
          s"""<tr>${row
            .map(r => s"""<td><pre>$r</pre></td>""")
            .mkString("")}</tr>"""
      )
      .mkString("")}
  </tbody>
</table></div>
        """
  }
  def toCsv = {
    tableHeader.mkString(",") + "\r\n" + tableRows
      .map(_.mkString(","))
      .mkString("\r\n")
  }
}

case class CustomComponentInterface[D](
    serializer: D => String,
    fqcn: String,
    jsLibraryOnClassPath: Option[String] = None
)

case class Graph(
    nodeLocations: Frame[String, Int, Double],
    edges: Mat[Double],
    nodeClasses: Vec[Int],
    classLabels: Seq[(Int, String)],
    extraTextByRowIdx: Series[Int, String],
    edgeColors: Seq[(Double, org.nspl.Color)]
) {
  def toDTO = {
    import io.circe.syntax._
    import org.saddle.circe._
    val s1 = nodeLocations.asJson.noSpaces
    val s2 = edges.asJson.noSpaces
    val s3 = nodeClasses.asJson.noSpaces
    val s4 = classLabels.asJson.noSpaces
    val s5 = extraTextByRowIdx.asJson.noSpaces
    val s6 = edgeColors
      .map(v => (v._1, v._2.r, v._2.g, v._2.b, v._2.a))
      .asJson
      .noSpaces
    GraphDTO(s1, s2, s3, s4, s5, s6)
  }
}

sealed trait Frames
case class StringFrame(
    s: Frame[List[String], String, String],
    rowIxHeader: Option[Seq[String]],
    ignoreRowIx: Boolean
) extends Frames
case class DoubleFrame(
    s: Frame[List[String], String, Double],
    highlights: Option[(Double, Double)],
    rowIxHeader: Option[Seq[String]],
    ignoreRowIx: Boolean,
    roundTo: Int
) extends Frames

case class CopyData(data: String, fileName: String)

case class Item(
    title: String,
    body: String,
    png: Option[String],
    svg: Option[String],
    pdf: Option[String],
    table: Option[Table],
    code: String,
    javascriptCode: String => String,
    sectionStart: Boolean,
    width: Option[Int],
    frame: Option[Frames],
    graph: Option[Graph],
    sequences: Option[Seq[(String, String)]],
    inToc: Boolean,
    fitWidth: Boolean,
    interactiveScatter: Option[ScatterDescription]
) {
  val filename = title.replaceAll("\\s+", "-") + "_" + java.util.UUID
    .randomUUID()
    .toString

  def mergeComplexRowIx[T](
      f: Frame[List[String], String, T],
      header: Option[Seq[String]]
  ): Frame[Int, String, String] = {
    val width = f.rowIx.toSeq.map(_.size).maxOption.getOrElse(0)
    val expandedRowIxIntoFrame = header
      .getOrElse((0 until width).map(_.toString))
      .zipWithIndex
      .map {
        case (name, idx) =>
          val ix = f.rowIx
          s"$name" -> Series(
            ix,
            ix.toVec.map(
              list =>
                if (list.size > idx) list(idx)
                else ""
            )
          )

      }
      .toFrame
    val frameAsString = f.mapValues(_.toString)
    val f2 = expandedRowIxIntoFrame rconcat frameAsString
    f2.resetRowIndex
  }
  def copyData: Option[CopyData] =
    if (code.nonEmpty) Some(CopyData(code, filename))
    else if (table.nonEmpty) Some(CopyData(table.get.toCsv, filename + ".csv"))
    else if (sequences.nonEmpty)
      Some(
        CopyData(
          sequences.get.map(v => s">${v._1}\n${v._2}").mkString("\n"),
          filename + ".fasta"
        )
      )
    else if (frame.nonEmpty) Some({
      frame.get match {
        case StringFrame(f, header, _) =>
          CopyData(
            new String(
              org.saddle.csv.CsvWriter
                .writeFrameToArray(
                  mergeComplexRowIx(f, header),
                  withColIx = true,
                  withRowIx = true
                ),
              "UTF-8"
            ),
            filename + ".csv"
          )
        case DoubleFrame(f, _, header, _, _) =>
          CopyData(
            new String(
              org.saddle.csv.CsvWriter
                .writeFrameToArray(
                  mergeComplexRowIx(f, header),
                  withColIx = true,
                  withRowIx = true
                ),
              "UTF-8"
            ),
            filename + ".csv"
          )
      }
    })
    else None
}

case class Page(
    title: String = "",
    items: Seq[Item] = Nil,
    tags: Seq[String] = Nil
) {
  def ++(that: Page) = copy(items = items ++ that.items)
  def append(item: Item) = copy(items = items :+ item)
  def title(s: String) = copy(title = s)
  def withTags(tags: Seq[String]) = copy(tags = tags)
}

case class Document(
    headPage: Page = Page("Page", Nil),
    tailPages: Seq[Page] = Nil,
    globalScripts: Seq[String] = Nil,
    globalData: Map[String, String] = Map.empty,
    gitSha: Option[String] = None,
    sourceData: Option[Array[Byte]] = None,
    footerText: String = "",
    noToc: Boolean = false
) {
  def filterPagesByTags(pred: String => Boolean) = {
    if (headPage.tags.exists(pred))
      copy(tailPages = tailPages.filter(_.tags.exists(pred)))
    else {
      val pages = tailPages.filter(_.tags.exists(pred))
      if (pages.nonEmpty) copy(headPage = pages.head, tailPages = pages.tail)
      else copy(headPage = Page("Page", Nil), tailPages = Nil)
    }
  }
  def withoutTableOfContents = copy(noToc = true)
  def pageTitle(s: String) = copy(headPage = headPage.title(s))
  def allItems = pages.flatMap(_.items)
  def pages = (headPage +: tailPages).filter(_.items.nonEmpty)
  def withTags(tags: String*) = copy(headPage = headPage.withTags(tags))
  def addAsPage(title: String, that: Document) = copy(
    tailPages = tailPages :+ Page(title, that.allItems, that.headPage.tags),
    globalScripts = globalScripts ++ that.globalScripts,
    globalData = globalData ++ that.globalData,
    gitSha = gitSha.orElse(that.gitSha),
    sourceData = sourceData.orElse(that.sourceData),
    footerText = if (footerText.isEmpty) that.footerText else footerText
  )
  def concatPages(that: Document) =
    copy(
      tailPages = tailPages ++ that.pages,
      globalScripts = globalScripts ++ that.globalScripts,
      globalData = globalData ++ that.globalData,
      gitSha = gitSha.orElse(that.gitSha),
      sourceData = sourceData.orElse(that.sourceData),
      footerText = if (footerText.isEmpty) that.footerText else footerText
    )
  def ++(that: Document) =
    concatPages(that)
  def addToHeadPage(that: Document) =
    this.copy(
      headPage = Page(
        this.headPage.title,
        this.headPage.items ++ that.allItems,
        that.headPage.tags
      ),
      globalScripts = globalScripts ++ that.globalScripts,
      globalData = globalData ++ that.globalData,
      gitSha = gitSha.orElse(that.gitSha),
      sourceData = sourceData.orElse(that.sourceData),
      footerText = if (footerText.isEmpty) that.footerText else footerText
    )
  val instant = java.time.Instant.now
  def git(sha: String) = copy(gitSha = Some(sha))
  def withSource(data: Array[Byte]) = copy(sourceData = Some(data))
  def footer(text: String) = copy(footerText = text)
  def frameAsTable[RX, CX, T](
      title: String,
      frame: org.saddle.Frame[RX, CX, T],
      body: String = "",
      inToc: Boolean = true
  ) =
    item(
      title,
      body = body,
      table = Some(
        Table(
          tableHeader = "" +: "" +: frame.colIx.toSeq
            .map(_.toString),
          tableRows = frame.toRowSeq.zipWithIndex.map {
            case ((rowIx, row), rowIdx) =>
              (rowIdx + 1).toString +: rowIx.toString +: row.values.toSeq
                .map(_.toString)
          }
        )
      ),
      inToc = inToc
    )
  def frame[RX, CX, T](
      title: String,
      frame: org.saddle.Frame[RX, CX, T],
      body: String = "",
      highlights: Option[(Double, Double)] = None,
      roundTo: Int = 3,
      rowIxHeader: Option[List[String]] = None,
      ignoreRowIx: Boolean = false,
      inToc: Boolean = true
  )(implicit st: org.saddle.ST[T], num: NUM[T]) = {
    val f = st match {
      case ScalarTagDouble =>
        DoubleFrame(
          frame
            .mapColIndex(_.toString)
            .mapRowIndex { ix =>
              ix match {
                case ix: Product     => ix.productIterator.map(_.toString).toList
                case ix: Iterable[_] => ix.map(_.toString).toList
                case ix              => List(ix.toString)
              }
            }
            .asInstanceOf[Frame[List[String], String, Double]]
            .mapVec(_.roundTo(roundTo)),
          highlights,
          rowIxHeader,
          ignoreRowIx,
          roundTo
        )
      case st =>
        DoubleFrame(
          frame
            .mapColIndex(_.toString)
            .mapRowIndex { ix =>
              ix match {
                case ix: Product     => ix.productIterator.map(_.toString).toList
                case ix: Iterable[_] => ix.map(_.toString).toList
                case ix              => List(ix.toString)
              }
            }
            .mapValues(v => st.toDouble(v))
            .mapVec(_.roundTo(roundTo)),
          highlights,
          rowIxHeader,
          ignoreRowIx,
          roundTo
        )
    }
    item(
      title,
      body = body,
      frame = Some(
        f
      ),
      inToc = inToc
    )
  }
  def stringFrame[RX, CX](
      title: String,
      frame: org.saddle.Frame[RX, CX, String],
      body: String = "",
      rowIxHeader: Option[List[String]] = None,
      ignoreRowIx: Boolean = false,
      inToc: Boolean = true
  ) =
    item(
      title,
      body = body,
      frame = Some(
        StringFrame(
          frame
            .mapColIndex(_.toString)
            .mapRowIndex { ix =>
              ix match {
                case ix: Product     => ix.productIterator.map(_.toString).toList
                case ix: Iterable[_] => ix.map(_.toString).toList
                case ix              => List(ix.toString)
              }
            }
            .mapVec(_.fillNA(_ => "NA")),
          rowIxHeader,
          ignoreRowIx
        )
      ),
      inToc = inToc
    )

  def document(d: Document) = this addToHeadPage d

  def item(
      title: String,
      body: String = "",
      png: Option[String] = None,
      svg: Option[String] = None,
      pdf: Option[String] = None,
      table: Option[Table] = None,
      code: String = "",
      javascriptCode: String => String = _ => "",
      sectionStart: Boolean = false,
      width: Option[Int] = None,
      frame: Option[Frames] = None,
      graph: Option[Graph] = None,
      interactiveScatter: Option[ScatterDescription] = None,
      inToc: Boolean = true,
      fitWidth: Boolean = true,
      sequences: Option[Seq[(String, String)]] = None
  ) =
    copy(
      headPage = headPage.append(
        Item(
          title,
          body,
          png,
          svg,
          pdf,
          table,
          code,
          javascriptCode,
          sectionStart,
          width,
          frame,
          graph,
          sequences,
          inToc,
          fitWidth,
          interactiveScatter
        )
      )
    )

  def javascript(
      title: String,
      javascriptCode: String => String,
      inToc: Boolean = true
  ) = item(title, javascriptCode = javascriptCode, inToc = inToc)

  def section(title: String, body: String = "", inToc: Boolean = true) =
    item(title, body = body, sectionStart = true, inToc = inToc)

  def libraryCode(code: String) = copy(globalScripts = globalScripts :+ code)
  def data(id: String, data: String) =
    copy(globalData = globalData.updated(id, data))

  def interactive[D](
      title: String,
      data: D,
      component: CustomComponentInterface[D]
  ) = {
    val id = java.util.UUID.randomUUID().toString
    this
      .data(id, component.serializer(data))
      .copy(
        globalScripts = globalScripts ++ component.jsLibraryOnClassPath.toList
          .map(classpath)
      )
      .javascript(
        title,
        elementId => s"""
    Stories.mountCustomComponent(
      getGlobalData('$id'),
      '${component.fqcn}',
      document.getElementById('$elementId')
    );
    """
      )
  }

  def png[T <: org.nspl.Renderable[T]](
      title: String,
      t: org.nspl.Build[T],
      body: String = "",
      table: Option[Table] = None,
      code: String = "",
      width: Int = 2000,
      cellWidth: Option[Int] = None,
      inToc: Boolean = true,
      fitWidth: Boolean = true
  )(implicit renderer: org.nspl.Renderer[T, org.nspl.JavaRC]) = {
    import org.nspl.awtrenderer._
    val png = pngToBase64String(
      t.build,
      width = width
    )
    item(
      title,
      body,
      Some(png),
      None,
      None,
      table,
      code,
      width = cellWidth,
      inToc = inToc,
      fitWidth = fitWidth
    )
  }

  def svg[T <: org.nspl.Renderable[T]](
      title: String,
      t: org.nspl.Build[T],
      body: String = "",
      table: Option[Table] = None,
      code: String = "",
      cellWidth: Option[Int] = None,
      inToc: Boolean = true,
      fitWidth: Boolean = true,
      drawTextAsCurves: Boolean = false,
      pdf: Boolean = true
  )(implicit renderer: org.nspl.Renderer[T, org.nspl.JavaRC]) = {
    import org.nspl.awtrenderer._
    val svg = svgToBase64String(
      t.build,
      width = 2000,
      drawTextAsCurves = drawTextAsCurves
    )
    val pdfAsString =
      if (pdf)
        Some(
          java.util.Base64.getEncoder.encodeToString(
            pdfToByteArray(
              t.build,
              width = 2000,
              drawTextAsCurves = drawTextAsCurves
            )
          )
        )
      else None
    item(
      title,
      body,
      None,
      Some(svg),
      pdfAsString,
      table,
      code,
      width = cellWidth,
      inToc = inToc,
      fitWidth = fitWidth
    )
  }

  def sequences(
      title: String,
      sequences: Seq[(String, String)],
      body: String = "",
      table: Option[Table] = None,
      inToc: Boolean = true
  ) =
    item(
      title,
      body = body,
      table = table,
      sequences = Some(
        sequences
      ),
      inToc = inToc
    )
  def graph(
      title: String,
      nodeLocations: Frame[String, Int, Double],
      edges: Mat[Double],
      nodeClusters: Vec[Int],
      clusterLabels: Seq[(Int, String)],
      extraTextByRowIdx: Series[Int, String] = Series.empty,
      body: String = "",
      table: Option[Table] = None,
      width: Int = 1000,
      inToc: Boolean = true,
      edgeColors: Seq[(Double, org.nspl.Color)] = Nil
  ) =
    item(
      title,
      body = body,
      table = table,
      graph = Some(
        Graph(
          nodeLocations,
          edges,
          nodeClusters,
          clusterLabels,
          extraTextByRowIdx,
          edgeColors
        )
      ),
      width = Some(width),
      inToc = inToc
    )
  def interactiveScatter(
      title: String,
      scatterDescription: ScatterDescription,
      body: String = "",
      table: Option[Table] = None,
      width: Int = 1000,
      inToc: Boolean = true
  ) =
    item(
      title,
      body = body,
      table = table,
      interactiveScatter = Some(
        scatterDescription
      ),
      width = Some(width),
      inToc = inToc
    )
  def graph2(
      title: String,
      g: Graph,
      body: String = "",
      table: Option[Table] = None,
      width: Int = 1000,
      inToc: Boolean = true
  ) =
    item(
      title,
      body = body,
      table = table,
      graph = Some(
        g
      ),
      width = Some(width),
      inToc = inToc
    )

  def plot[T <: org.nspl.Renderable[T]](
      title: String,
      t: org.nspl.Build[T],
      body: String = "",
      table: Option[Table] = None,
      code: String = "",
      svg: Boolean = false,
      pdf: Boolean = false,
      width: Int = 2000,
      cellWidth: Option[Int] = None,
      inToc: Boolean = true,
      drawTextAsCurves: Boolean = false
  )(implicit renderer: org.nspl.Renderer[T, org.nspl.JavaRC]) = {
    import org.nspl.awtrenderer._
    if (!svg) {
      val png = pngToBase64String(
        t.build,
        width = width
      )
      val pdfAsString =
        if (pdf)
          Some(
            java.util.Base64.getEncoder.encodeToString(
              pdfToByteArray(
                t.build,
                width = 2000,
                drawTextAsCurves = drawTextAsCurves
              )
            )
          )
        else None
      item(
        title,
        body,
        Some(png),
        None,
        pdfAsString,
        table,
        code,
        width = cellWidth,
        inToc = inToc
      )
    } else {
      val svg = svgToBase64String(
        t.build,
        width = width,
        drawTextAsCurves = drawTextAsCurves
      )
      val pdfAsString =
        if (pdf)
          Some(
            java.util.Base64.getEncoder.encodeToString(
              pdfToByteArray(
                t.build,
                width = 2000,
                drawTextAsCurves = drawTextAsCurves
              )
            )
          )
        else None
      item(
        title,
        body,
        None,
        Some(svg),
        pdfAsString,
        table,
        code,
        width = cellWidth,
        inToc = inToc
      )
    }
  }

  def shortTable(
      title: String,
      table: Map[String, Any],
      body: String = "",
      code: String = "",
      inToc: Boolean = true
  ) = {
    item(
      title,
      body,
      table = Some(Table(Seq("Key", "Value"), table.toSeq.sortBy(_._1).map {
        case (key, value) => Seq(key, value.toString)
      })),
      inToc = inToc
    )
  }

  def render = renderWithOptions()

  def renderWithOptions(jsOnClassPath: Option[String] = None): String = {

    val jsCode = {
      val inputStream = getClass.getResourceAsStream(
        jsOnClassPath.getOrElse("/stories-js-opt.js")
      )
      val source = scala.io.Source.fromInputStream(inputStream)
      val str = source.mkString
      source.close
      str
    }

    def tocData(page: Int) = {
      val numbers = pages(page).items
        .scanLeft(1)(
          (num, item) =>
            if (item.sectionStart) num
            else num + 1
        )
        .dropRight(1)

      pages(page).items.zipWithIndex
        .zip(numbers)
        .filter(_._1._1.inToc)
        .map {
          case ((item, itemIdx), num) =>
            val idx = page * 10000 + itemIdx
            val ref = s"#storycard-main-${idx}"
            val title = s"${if (item.sectionStart) ""
            else s"$num."} ${item.title}"
            (title, ref, item.sectionStart)
        }

    }

    def renderedGlobalData = globalData.iterator.map {
      case (id, data) =>
        s"""<script id="$id" type="application/octet-stream">$data</script>"""
    }
    def renderedGlobalScripts = globalScripts.iterator.map { script =>
      s"""<script>$script</script>"""
    }

    def renderItem(i: Item, idx: Int, displayedNumber: Int) = {
      val copyData =
        s"""<script id="__stories_item_copy_data_${idx}__" type="application/octet-stream">${i.copyData
          .map(_.data)
          .getOrElse("")}</script>
     """
      val copyFragment = i.copyData
        .map(
          _ =>
            s"""<button class="btn btn-sm btn-outline" type="button" onclick="navigator.clipboard.writeText(getGlobalData('__stories_item_copy_data_${idx}__'))">copy to clipboard</button>"""
        )
        .getOrElse("")
      val saveFragment = i.copyData
        .map(
          data =>
            s"""<button class="btn btn-sm btn-outline" type="button" onclick="downloadData(getGlobalData('__stories_item_copy_data_${idx}__'),'${data.fileName}')">save to file</button>"""
        )
        .getOrElse("")

      val pdfData = i.pdf
        .map { pdf =>
          s"""
            <script type="application/octet-stream" id="storycard-$idx-pdf-data">$pdf</script>
           
            """

        }
        .getOrElse("")
      val pdfFragment = i.pdf
        .map { pdf =>
          s"""
            
            <button class="btn btn-sm btn-outline" type="button" onclick="downloadDataB64(getGlobalData('storycard-$idx-pdf-data'),'${i.filename}.pdf')">
            save to pdf</button>
            """

        }
        .getOrElse("")

      if (i.sectionStart == true)
        s"""
      <div id="storycard-main-$idx">
      <h3>${i.title}</h3>
      <div class="markdown-body">${markdown2html(i.body)}</div>
      </div>
      """
      else
        s"""            
        
<div id="storycard-main-$idx" class="Box mb-3" >
$copyData
$pdfData

<div class="Box-header"><h5 class="Box-title">${displayedNumber}. ${i.title} $copyFragment $saveFragment $pdfFragment</h5></div>
<div id="storycard-$idx" class="Box-body" style="border-bottom: unset; width:${i.width
          .map(_ + "px")
          .getOrElse("auto")}">
<div class="markdown-body">${markdown2html(i.body)}</div>
${i.png.map(
            png =>
              s"""<img src="data:image/png;base64,$png" class="${if (i.fitWidth)
                "width-fit"
              else ""}">"""
          )
          .getOrElse("")}
${i.svg.map { svg =>
            s"""<img src="data:image/svg+xml;base64,$svg" class="${if (i.fitWidth)
              "width-fit"
            else ""}">"""
          }
          .getOrElse("")}

${i.table.map(_.render).getOrElse("")}
${i.frame.map { frame =>
            val serialized = {
              import io.circe.syntax._
              import org.saddle.circe._
              frame match {
                case StringFrame(f, rowIxHeader, _) =>
                  (rowIxHeader, f).asJson.noSpaces
                case DoubleFrame(f, _, rowIxHeader, _, _) =>
                  (rowIxHeader, f).asJson.noSpaces
              }

            }
            val data = s"""<div id="storycard-$idx-frame"></div>
            <script type="application/octet-stream" id="storycard-$idx-frame-data">$serialized</script>"""
            val div = frame match {
              case StringFrame(f, rowIxHeader, ignoreRowIx) =>
                s"""<script>Stories.mountStringTable(document.getElementById("storycard-$idx-frame"),document.getElementById("storycard-$idx-frame-data").textContent, $ignoreRowIx)</script>"""
              case DoubleFrame(f, None, rowIxHeader, ignoreRowIx, roundTo) =>
                s"""<script>Stories.mountDoubleTable(document.getElementById("storycard-$idx-frame"),document.getElementById("storycard-$idx-frame-data").textContent, $ignoreRowIx,$roundTo)</script>"""
              case DoubleFrame(
                  f,
                  Some((low, high)),
                  rowIxHeader,
                  ignoreRowIx,
                  roundTo
                  ) =>
                s"""<script>Stories.mountDoubleTableWithHighlights(document.getElementById("storycard-$idx-frame"),document.getElementById("storycard-$idx-frame-data").textContent,$low,$high, $ignoreRowIx, $roundTo)</script>"""
            }
            data + "\n" + div
          }
          .getOrElse("")}
${i.sequences
          .map { sequences =>
            val serialized = {
              import io.circe.syntax._
              import org.saddle.circe._
              sequences.asJson.noSpaces

            }
            val data = s"""<div id="storycard-$idx-sequences"></div>
            <script type="application/octet-stream" id="storycard-$idx-sequences-data">$serialized</script>"""
            val div =
              s"""<script>Stories.mountSequenceViewerSerialized(document.getElementById("storycard-$idx-sequences"),document.getElementById("storycard-$idx-sequences-data").textContent)</script>"""

            data + "\n" + div
          }
          .getOrElse("")}

${i.graph.map { graph =>
            import io.circe.syntax._
            import org.saddle.circe._
            val s1 = graph.nodeLocations.asJson.noSpaces
            val s2 = graph.edges.asJson.noSpaces
            val s3 = graph.nodeClasses.asJson.noSpaces
            val s4 = graph.classLabels.asJson.noSpaces
            val s5 = graph.extraTextByRowIdx.asJson.noSpaces
            val s6 = graph.edgeColors
              .map(v => (v._1, v._2.r, v._2.g, v._2.b, v._2.a))
              .asJson
              .noSpaces

            val data = s"""<div id="storycard-$idx-graph"></div>
<script type="application/octet-stream" id="storycard-$idx-graph-data1">$s1</script>
<script type="application/octet-stream" id="storycard-$idx-graph-data2">$s2</script>
<script type="application/octet-stream" id="storycard-$idx-graph-data3">$s3</script>
<script type="application/octet-stream" id="storycard-$idx-graph-data4">$s4</script>
<script type="application/octet-stream" id="storycard-$idx-graph-data5">$s5</script>
<script type="application/octet-stream" id="storycard-$idx-graph-data6">$s6</script>
"""
            val div =
              s"""<script>Stories.mountInteractiveGraphPlot(document.getElementById("storycard-$idx-graph"),document.getElementById("storycard-$idx-graph-data1").textContent,document.getElementById("storycard-$idx-graph-data2").textContent,document.getElementById("storycard-$idx-graph-data3").textContent,document.getElementById("storycard-$idx-graph-data4").textContent,document.getElementById("storycard-$idx-graph-data5").textContent,document.getElementById("storycard-$idx-graph-data6").textContent, ${i.width
                .getOrElse(1000)})</script>"""

            data + "\n" + div
          }
          .getOrElse("")}
${i.interactiveScatter
          .map { scatterDescription =>
            import io.circe.syntax._
            import org.saddle.circe._
            val s1 = scatterDescription.asJson.noSpaces

            val data = s"""<div id="storycard-$idx-scatter"></div>
<script type="application/octet-stream" id="storycard-$idx-scatter-data1">$s1</script>

"""
            val div =
              s"""<script>Stories.mountInteractiveScatters(document.getElementById("storycard-$idx-scatter"),document.getElementById("storycard-$idx-scatter-data1").textContent, ${i.width
                .getOrElse(1000)})</script>"""

            data + "\n" + div
          }
          .getOrElse("")}
${if (i.code.nonEmpty)
          s"""<pre class="pre-scrollable">${i.code}</pre>"""
        else ""}
</div>
</div>
"""
    }

    val renderedPages = pages.zipWithIndex.map {
      case (page, pageIdx) =>
        val numbers = page.items
          .scanLeft(1)(
            (num, item) =>
              if (item.sectionStart) num
              else num + 1
          )
          .dropRight(1)
        assert(numbers.size == page.items.size)

        (
          page,
          pageIdx,
          tocData(pageIdx),
          page.items.zipWithIndex
            .zip(numbers)
            .map {
              case ((item, itemIdx), itemNum) =>
                val idx = pageIdx * 10000 + itemIdx
                val rendered = renderItem(item, idx, itemNum)
                val scriptTag = item.javascriptCode(s"storycard-$idx")
                (rendered, scriptTag)
            }
        )
    }

    val renderedSourceData =
      s"""<script id="__stories_source_data__" type="application/octet-stream">${sourceData
        .map(b => java.util.Base64.getEncoder.encodeToString(b))
        .getOrElse("")}</script>
     """

    val template = s"""
<!doctype html>
<html lang="en">
  <head>
    <!-- Required meta tags -->
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <!-- Primer CSS -->
    ${Document.styleSheets}

  </head>
  <body>
  <!-- Scala-JS, used to render some nodes below --!>
   <script>${jsCode}</script>
  

  ${renderedPages
      .map {
        case (page, pageIdx, tocData, renderedItems) =>
          s"""
     <div style="display:none;" id="__page$pageIdx">
      <div class="d-flex flex-row flex-nowrap flex-justify-start flex-items-stretch">
        <div id="__toc$pageIdx"></div>
        <div style="margin-right: 60px; margin-left: 60px; max-width: calc(100vw - 420px)">${renderedItems
            .map(_._1)
            .mkString("\n")}</div>
      </div>
    </div>
    """

      }
      .mkString("\n\n")}
 

  <div id="tabroot"></div>

  <footer class="d-flex flex-justify-around" style="margin-left:420px;margin-bottom:10px">
  <div class="col-sm" >
      <span class="Label Label--outline">${instant}</span>
    </div>
    <div class="col-sm">
      <span class="Label Label--outline">${gitSha.getOrElse("NA")}</span>
    </div>
    <div class="col-sm">
      <button type="button" class="btn-link" onclick="downloadSource();">Download source</button>
    </div>
    <div class="col-sm">
       <span class="text-small">$footerText</span>
    </div>
  </footer>

    <!-- Optional JavaScript -->
    ${Document.scripts}
    
    <!-- data -->
    $renderedSourceData
    ${renderedGlobalData.mkString("\n")}
    <script>
      function getGlobalData(id) {
        return document.getElementById(id).textContent
      }
      // adapted from MDN
      function atobUTF16 (sBase64) {
        var sBinaryString = atob(sBase64), aBinaryView = new Uint8Array(sBinaryString.length);
        Array.prototype.forEach.call(aBinaryView, function (el, idx, arr) { arr[idx] = sBinaryString.charCodeAt(idx); });
        return aBinaryView
      }
      function downloadSource() {
        var data = atobUTF16(getGlobalData("__stories_source_data__"));
        if (data.length > 0) {
        var file = new Blob([data],{type: "application/octet-stream"});
        var a = document.createElement("a"), url = URL.createObjectURL(file);
        a.href = url;
        a.download = "source_${gitSha.getOrElse("NA")}_${instant}.zip";
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
            document.body.removeChild(a);
            window.URL.revokeObjectURL(url);  
        }, 0); 
      }
      }
      function downloadData(data,filename) {
        if (data.length > 0) {
        var file = new Blob([data],{type: "application/octet-stream"});
        var a = document.createElement("a"), url = URL.createObjectURL(file);
        a.href = url;
        a.download = filename;
        
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
            document.body.removeChild(a);
            window.URL.revokeObjectURL(url);  
        }, 0); 
      }
      }
      function downloadDataB64(encoded,filename) {
        var data = atobUTF16(encoded);
        if (data.length > 0) {
        var file = new Blob([data],{type: "application/octet-stream"});
        var a = document.createElement("a"), url = URL.createObjectURL(file);
        a.href = url;
        a.download = filename;
        
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
            document.body.removeChild(a);
            window.URL.revokeObjectURL(url);  
        }, 0); 
      }
      }
    </script>
    <!-- global library scripts -->
    ${renderedGlobalScripts.mkString("\n")}
    <!-- card scripts -->
    <script>
        window.onload = function() {  
            ${renderedPages.flatMap(_._4.map(_._2)).mkString("\n")}


            var root = document.getElementById("tabroot")


            ${pages.zipWithIndex
      .map {
        case (page, idx) =>
          s"""
            var page$idx = document.getElementById("__page$idx");
            """
      }
      .mkString("\n")}

         ${if (!noToc)
      renderedPages
        .map {
          case (_, pageIdx, tocData, _) =>
            s"Stories.mountTableOfContents(document.getElementById('__toc$pageIdx'),${tocData
              .map(v => "'" + v._1 + "'")
              .mkString("[", ",", "]")},${tocData
              .map(v => "'" + v._2 + "'")
              .mkString("[", ",", "]")},${tocData.map(v => v._3).mkString("[", ",", "]")});"

        }
        .mkString("\n")
    else ""}

            
            Stories.mountTabNavigation(root,${pages
      .map(s => s""" "${s.title}" """)
      .mkString("[", ",", "]")},${(0 until pages.size)
      .map(i => s"page$i")
      .mkString("[", ",", "]")})

   

        }
    </script>

    
   
   

    <style>
    table {
    width: 100%;
    text-align: left;
    }
    table td, table th {
    padding: .75rem;
    vertical-align: top;
    border-top: 1px solid #dee2e6;
}

table td {
  background-color: #ffffff !important;
}

table thead th {
    vertical-align: bottom;
    border-bottom: 2px solid #dee2e6;
    border-top: 0px;
    position: -webkit-sticky !important;
    position: sticky !important;
    top: 37px;
    background-color: #fafbfc !important;
}
</style>

  </body>
</html>
        """

    template
  }
}

object Document {
  val scripts = List().map(scr => s"""<script>$scr</script>""").mkString("\n")
  val styleSheets = List(
    classpath("/primer.css")
  ).map(css => s"""<style  type="text/css">$css</style>""").mkString("\n")
}
