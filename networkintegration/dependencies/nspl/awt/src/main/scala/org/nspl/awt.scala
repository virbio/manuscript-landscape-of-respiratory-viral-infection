package org.nspl

import java.awt.Graphics2D
import java.awt.{Font => JFont}
import java.text.AttributedString
import java.awt.font.LineBreakMeasurer

import JavaFontConversion._
import java.awt.font.TextAttribute

case class JavaRC(graphics: Graphics2D, drawTextAsCurves: Boolean)
    extends RenderingContext[JavaRC] {

  var paintColor: Color = Color.black
  var stroke: Stroke = Stroke(1d fts)
  var transform: AffineTransform = AffineTransform.identity
  var transformInGraphics: AffineTransform = AffineTransform.identity

  def withPaint[T](color: Color)(f: => T) = {
    val current = paintColor
    if (current != color) {
      paintColor = color
      graphics.setPaint(awtrenderer.col2col(color))
    }
    f
  }
  def withStroke[T, F: FC](str: Stroke)(f: => T) = {
    val current = stroke
    if (current != str) {
      stroke = str
      graphics.setStroke(awtrenderer.str2str(str))
    }
    f
  }

  type LocalTx = AffineTransform

  override def getTransform: AffineTransform = transform
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
      graphics.setTransform(awtrenderer.tx2tx(transform))
    }
  }

}

object awtrenderer extends JavaAWTUtil {

  implicit val defaultGlyphMeasurer = AwtGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = font("sans-serif")

  implicit val shapeRenderer = new Renderer[ShapeElem, JavaRC] {

    private def drawAndFill(ctx: JavaRC, elem: ShapeElem) = {

      if (elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)) {
        ctx.setTransformInGraphics()

        val shape = elem.shape

        if (elem.fill.a > 0.0) {
          ctx.withPaint(elem.fill) {
            ctx.graphics.fill(shape2awt(shape))
          }
        }
        if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
          ctx.withPaint(elem.strokeColor) {
            // val stroke = ctx.graphics match {
            //   case _: de.erichseifert.vectorgraphics2d.VectorGraphics2D =>
            //     elem.stroke
            //       .map(s => Stroke(s.width * 0.4, s.cap, s.dashArray))
            //       .get
            //   case _ => elem.stroke.get
            // }
            ctx.withStroke(elem.stroke.get) {
              ctx.graphics.draw(shape2awt(shape))
            }
          }
        }
      }
    }
    def render(ctx: JavaRC, elem: ShapeElem): Unit = {

      ctx.withTransform(elem.tx concat elem.shape.currentTransform) {

        drawAndFill(ctx, elem)

      }

    }
  }

  implicit val textRenderer = new Renderer[TextBox, JavaRC] {

    def getTextLayouts(
        text: String,
        font: java.awt.Font,
        bold: Boolean,
        oblique: Boolean,
        subScript: Boolean,
        superScript: Boolean,
        underline: Boolean,
        graphics: Graphics2D
    ): java.awt.Shape = {
      val attributedString = new AttributedString(text)

      val derivedFont =
        if (!bold && !oblique && !subScript && !superScript)
          font
        else {
          val map = scala.collection.mutable.Map[TextAttribute, Any]()

          if (bold) {
            map.update(
              java.awt.font.TextAttribute.WEIGHT,
              TextAttribute.WEIGHT_EXTRABOLD
            )
          }
          if (oblique) {
            map.update(
              java.awt.font.TextAttribute.POSTURE,
              TextAttribute.POSTURE_OBLIQUE
            )
          }
           if (underline) {
                map.update(
                  java.awt.font.TextAttribute.UNDERLINE,
                  TextAttribute.UNDERLINE_ON
                )
              }
          if (subScript && !superScript) {
            map.update(
              java.awt.font.TextAttribute.SUPERSCRIPT,
              TextAttribute.SUPERSCRIPT_SUB
            )
          }
          if (!subScript && superScript) {
            map.update(
              java.awt.font.TextAttribute.SUPERSCRIPT,
              TextAttribute.SUPERSCRIPT_SUPER
            )
          }
          import scala.jdk.CollectionConverters._
          font.deriveFont(map.asJava)
        }
      attributedString.addAttribute(java.awt.font.TextAttribute.FONT, font);

      val paragraph = attributedString.getIterator();
      val paragraphStart = paragraph.getBeginIndex();
      val paragraphEnd = paragraph.getEndIndex();
      val frc = graphics.getFontRenderContext();
      val lineMeasurer = new LineBreakMeasurer(paragraph, frc);
      lineMeasurer.setPosition(paragraphStart);

      val layout = lineMeasurer.nextLayout(Float.MaxValue);
      val shape = layout.getOutline(new java.awt.geom.AffineTransform)

      shape

    }

    def render(ctx: JavaRC, elem: TextBox): Unit = {
      if (!elem.layout.isEmpty && elem.color.a > 0) {
        ctx.withTransform(elem.txLoc) {
          ctx.withPaint(elem.color) {

            if (!elem.bold && !elem.oblique && !elem.subScript && !elem.superScript && !elem.underline) {
              ctx.graphics.setFont(font2font(elem.font))
            } else {

              val map =
                scala.collection.mutable.Map[TextAttribute, Any]()

              if (elem.bold) {
                map.update(
                  java.awt.font.TextAttribute.WEIGHT,
                  TextAttribute.WEIGHT_EXTRABOLD
                )
              }
              if (elem.oblique) {
                map.update(
                  java.awt.font.TextAttribute.POSTURE,
                  TextAttribute.POSTURE_OBLIQUE
                )
              }
              if (elem.underline) {
                map.update(
                  java.awt.font.TextAttribute.UNDERLINE,
                  TextAttribute.UNDERLINE_ON
                )
              }
              if (elem.subScript && !elem.superScript) {
                map.update(
                  java.awt.font.TextAttribute.SUPERSCRIPT,
                  TextAttribute.SUPERSCRIPT_SUB
                )
              }
              if (!elem.subScript && elem.superScript) {
                map.update(
                  java.awt.font.TextAttribute.SUPERSCRIPT,
                  TextAttribute.SUPERSCRIPT_SUPER
                )
              }
              import scala.jdk.CollectionConverters._
              val derivedFont = font2font(elem.font).deriveFont(map.asJava)
              ctx.graphics.setFont(derivedFont)
            }

            elem.layout.lines.foreach {
              case (line, lineTx) =>
                ctx.withTransform(lineTx) {
                  ctx.setTransformInGraphics()

                  if (ctx.drawTextAsCurves) {
                    val shape =
                      getTextLayouts(
                        text = line,
                        font = font2font(elem.font),
                        bold = elem.bold,
                        oblique = elem.oblique,
                        subScript = elem.subScript,
                        superScript = elem.superScript,
                        underline = elem.underline,
                        graphics = ctx.graphics
                      )
                    ctx.graphics.fill(shape)
                  } else {
                    ctx.graphics.drawString(line, 0, 0)
                  }
                }
            }
          }

        }

      }
    }
  }

}
