package org.nspl

import java.awt.{Graphics2D, Font => JFont}
import java.awt.event._
import javax.swing.event.MouseInputAdapter
import java.io._

trait JavaAWTUtil {

  import org.nspl.JavaFontConversion._

  private[nspl] def shape2awt(s: Shape): java.awt.Shape = s match {
    case Rectangle(x, y, w, h, tx, _) =>
      new java.awt.geom.Rectangle2D.Double(x, y, w, h)

    case Ellipse(x, y, w, h, tx) =>
      new java.awt.geom.Ellipse2D.Double(x, y, w, h)

    case Line(x1, y1, x2, y2, tx) =>
      new java.awt.geom.Line2D.Double(x1, y1, x2, y2)

    case SimplePath(points, tx) => {
      val path = new java.awt.geom.GeneralPath()
      path.moveTo(points.head.x, points.head.y)
      points.drop(1).foreach { p =>
        path.lineTo(p.x, p.y)
      }
      path.closePath
      path
    }
    case Path(ops, tx) => {
      val path = new java.awt.geom.GeneralPath()
      ops foreach {
        case MoveTo(Point(x, y)) => path.moveTo(x, y)
        case LineTo(Point(x, y)) => path.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          path.quadTo(x1, y1, x2, y2)
        case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) =>
          path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      path.closePath
      path
    }
  }

  private[nspl] def col2col(c: Color): java.awt.Paint =
    new java.awt.Color(c.r, c.g, c.b, c.a)

  private[nspl] def str2str[F: FC](s: Stroke) =
    new java.awt.BasicStroke(
      s.width.value.toFloat,
      s.cap match {
        case CapButt   => java.awt.BasicStroke.CAP_BUTT
        case CapSquare => java.awt.BasicStroke.CAP_SQUARE
        case CapRound  => java.awt.BasicStroke.CAP_ROUND
      },
      java.awt.BasicStroke.JOIN_MITER,
      1f,
      if (s.dashArray.count(_ > 0) == 0) null
      else s.dashArray.map(_.toFloat).toArray,
      0f
    )

  private[nspl] def rec2bounds(r: java.awt.geom.Rectangle2D) =
    Bounds(r.getX, r.getY, r.getWidth, r.getHeight)

  private[nspl] def bounds2rec(r: Bounds) =
    new java.awt.geom.Rectangle2D.Double(r.x, r.y, r.w, r.h)

  private[nspl] def tx2tx(tx: AffineTransform): java.awt.geom.AffineTransform =
    new java.awt.geom.AffineTransform(
      tx.m0,
      tx.m3,
      tx.m1,
      tx.m4,
      tx.m2,
      tx.m5
    )

  def show[K <: Renderable[K]](
      build0: Build[K],
      drawTextAsCurves: Boolean = false
  )(
      implicit er: Renderer[K, JavaRC]
  ): (javax.swing.JFrame, Build[K] => Unit) = {
    import javax.swing._
    import java.awt.{Graphics, RenderingHints}
    val frame = new JFrame("");
    var build = build0
    var paintableElem = build.build

    val update = (k: Build[K]) => {
      synchronized {
        build = k
        paintableElem = build.build
      }
      frame.repaint()
    }

    frame.setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE);
    frame
      .getContentPane()
      .add(
        new JComponent {
          override def paintComponent(g: Graphics) = {
            super.paintComponent(g)
            val g2 = g.asInstanceOf[Graphics2D]
            val renderingContext = JavaRC(g2, drawTextAsCurves)
            val bounds = getBounds()

            renderingContext
              .render(fitToBounds(paintableElem, rec2bounds(bounds)))
          }
        },
        java.awt.BorderLayout.CENTER
      );

    val d = new java.awt.Dimension(
      (paintableElem.bounds.w * 3).toInt,
      (paintableElem.bounds.h * 3).toInt
    )
    frame.setSize(d);
    frame.setVisible(true);
    (frame, update)
  }

  def writeVector[K <: Renderable[K]](
      build: Build[K],
      os: java.io.OutputStream,
      drawTextAsCurves: Boolean = false,
      width: Int = 500,
      format: String = "pdf"
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    import de.erichseifert.vectorgraphics2d._
    import de.erichseifert.vectorgraphics2d.pdf._
    import de.erichseifert.vectorgraphics2d.svg._
    import de.erichseifert.vectorgraphics2d.eps._
    import util._

    val elem = build.build

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt
    val g2d = new VectorGraphics2D()
    val renderingContext = JavaRC(g2d, drawTextAsCurves)

    val processor =
      format match {
        case "pdf" => new PDFProcessor()
        case "svg" => new SVGProcessor()
        case "eps" => new EPSProcessor()
      }
    val bounds = Bounds(0, 0, width, height)
    renderingContext.render(fitToBounds(elem, bounds))

    val document =
      processor.getDocument(g2d.getCommands, new PageSize(width, height))
    document.writeTo(os)

  }

  def write[K <: Renderable[K]](
      elem: Build[K],
      os: java.io.OutputStream,
      drawTextAsCurves: Boolean = false,
      width: Int = 1000,
      mimeType: String = "image/png"
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    mimeType.split("/").last match {
      case "pdf" | "svg" | "svg+xml" | "eps" =>
        writeVector(elem, os, drawTextAsCurves, width, mimeType.split("/").last)
      case _ => writeBitmap(elem, os, width, mimeType)
    }
  }

  def writeBitmap[K <: Renderable[K]](
      build: Build[K],
      os: java.io.OutputStream,
      width: Int = 1000,
      mimeType: String = "image/png"
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    import java.awt.image.BufferedImage

    import javax.imageio.ImageIO;
    import javax.imageio.ImageWriter;
    import javax.imageio.stream.ImageOutputStream;
    import java.awt.{Graphics, RenderingHints}

    val elem = build.build

    val aspect = elem.bounds.h / elem.bounds.w
    val height = math.max((width * aspect).toInt, 1)

    val bimage = new BufferedImage(
      width,
      height,
      BufferedImage.TYPE_INT_ARGB
    );

    val g2d = bimage.createGraphics();
    val renderingContext = JavaRC(g2d, false)

    g2d.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    );
    g2d.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON
    );

    val bounds = Bounds(0, 0, width, height)
    renderingContext.render(fitToBounds(elem, bounds))

    {
      val imageWriter = ImageIO.getImageWritersByMIMEType(mimeType).next

      val ios = ImageIO.createImageOutputStream(os);
      imageWriter.setOutput(ios)
      try {
        imageWriter.write(bimage);
      } finally {
        ios.close();
      }

    }

  }

  private def writeBinaryToFile(f: File, data: Array[Byte]): Unit = {
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      os.write(data)
    } finally {
      os.close
    }
  }

  def renderToByteArray[K <: Renderable[K]](
      elem: Build[K],
      drawTextAsCurves: Boolean = false,
      width: Int = 1000,
      mimeType: String = "image/png"
  )(
      implicit er: Renderer[K, JavaRC]
  ): Array[Byte] = {
    val bs = new java.io.ByteArrayOutputStream()
    write(elem, bs, drawTextAsCurves, width, mimeType);
    bs.toByteArray
  }

  def renderToFile[K <: Renderable[K]](
      elem: Build[K],
      drawTextAsCurves: Boolean = false,
      width: Int = 1000,
      mimeType: String = "image/png"
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    val f = java.io.File.createTempFile("nspl", "." + mimeType.split("/").last)
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      write(elem, os, drawTextAsCurves, width, mimeType)
    } finally {
      os.close
    }
    f
  }

  def renderToFile[K <: Renderable[K]](
      f: File,
      elem: Build[K],
      drawTextAsCurves: Boolean,
      width: Int,
      mimeType: String
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = {
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      write(elem, os, drawTextAsCurves, width, mimeType)
    } finally {
      os.close
    }
    f
  }

  def svgToFile[K <: Renderable[K]](
      f: File,
      elem: Build[K],
      width: Int,
      drawTextAsCurves: Boolean
  )(
      implicit
      er: Renderer[K, JavaRC]
  ): File = {
    renderToFile(f, elem, drawTextAsCurves, width, "svg")
    f
  }
  def pdfToFile[K <: Renderable[K]](
      f: File,
      elem: Build[K],
      width: Int,
      drawTextAsCurves: Boolean
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = {
    renderToFile(f, elem, drawTextAsCurves, width, "application/pdf")
    f
  }

  def pdfToFile[K <: Renderable[K]](
      f: File,
      elem: K,
      drawTextAsCurves: Boolean 
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = pdfToFile(f, elem, 500, drawTextAsCurves)

  def pdfToFile[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 500,
      drawTextAsCurves: Boolean = false
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    val f = java.io.File.createTempFile("nspl", ".pdf")
    renderToFile(f, elem, drawTextAsCurves, width, "application/pdf")
    f
  }

  def pngToFile[K <: Renderable[K]](
      f: File,
      elem: Build[K],
      width: Int
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = {
    renderToFile(f, elem, false, width, "image/png")
    f
  }

  def pngToFile[K <: Renderable[K]](
      f: File,
      elem: K
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = pngToFile(f, elem, 1000)

  def pngToFile[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000
  )(
      implicit er: Renderer[K, JavaRC]
  ): File = {
    renderToFile(elem, false, width, "image/png")
  }
  def pngToByteArray[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000
  )(
      implicit
      er: Renderer[K, JavaRC]
  ): Array[Byte] = {
    renderToByteArray(elem, false, width, "image/png")
  }

  def pdfToByteArray[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000,
      drawTextAsCurves: Boolean = false
  )(
      implicit
      er: Renderer[K, JavaRC]
  ): Array[Byte] = {
    renderToByteArray(elem, drawTextAsCurves, width, "application/pdf")
  }
  def svgToByteArray[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000,
      drawTextAsCurves: Boolean = false
  )(
      implicit
      er: Renderer[K, JavaRC]
  ): Array[Byte] = {
    renderToByteArray(elem, drawTextAsCurves, width, "svg")
  }

  def pngToBase64String[K <: Renderable[K]](
      elem: K,
      width: Int = 1000
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    val os = new ByteArrayOutputStream()
    try {
      write(elem, os, false, width, "image/png")
    } finally {
      os.close
    }
    java.util.Base64.getEncoder.encodeToString(os.toByteArray)
  }

  def svgToBase64String[K <: Renderable[K]](
      elem: K,
      drawTextAsCurves: Boolean,
      width: Int = 1000
  )(
      implicit er: Renderer[K, JavaRC]
  ) = {
    val os = new ByteArrayOutputStream()
    try {
      writeVector(elem, os, drawTextAsCurves, width, "svg")
    } finally {
      os.close
    }
    java.util.Base64.getEncoder.encodeToString(os.toByteArray)
  }

}
