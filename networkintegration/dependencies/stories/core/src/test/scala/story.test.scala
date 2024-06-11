package stories

import org.scalatest.funsuite.AnyFunSuite
import org.saddle.order._

class StorySuite extends AnyFunSuite {

  test("should render") {
    import org.nspl._
    import org.nspl.awtrenderer._
    import org.saddle._
    val png = {
      pngToBase64String(
        xyplot(indexed(Seq(1, 2, 3)))(par()).build,
        width = 2000
      )
    }
    val plot = xyplot(indexed(Seq(1, 2, 3)))(par())
    val html = Document()
      .interactive[String](
        "interactive",
        "boo",
        CustomComponentInterface(identity, "stories.TestComponent", None)
      )
      .item(
        "blah blah",
        """# lorem ipum dolor
        something something
        
        1. b1
        2. b2
        
        bye""",
        table =
          Some(Table(List("a", "b"), List(List("c", "d"), List("e", "f"))))
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item(
        "blah blah sdfs dsf sdf sad fdsa fds fdsf d dsf sdfsdafds sdf sf sdfads fsdf ad ds fadsds fsdfsda ds ds fds ds fdsa fsd fds fsd fsda fdsa fds ",
        "lorem ipum dolor"
      )
      .item("blah blah", "lorem ipum dolor", png = Some(png), code = "1 2 3")
      .section("new section", body = "blah blah blah. blah blah blh")
      .addAsPage(
        "newpage",
        Document()
          .interactiveScatter(
            "scatter",
            scatterDescription = ScatterDescription(
              Vector(
                ScatterPanelDescription(
                  data = Frame(
                    0 -> Series("a" -> 1d, "b" -> 2d),
                    1 -> Series("a" -> 2d, "b" -> 3d),
                    2 -> Series("a" -> 3d, "b" -> 4d)
                  ),
                  main = "boo"
                ),
                ScatterPanelDescription(
                  data = Frame(
                    0 -> Series("a" -> 1d, "b" -> 2d),
                    1 -> Series("a" -> 2d, "b" -> 3d),
                    2 -> Series("a" -> 3d, "b" -> 4d)
                  ),
                  main = "boo"
                )
              ),
              rowDescriptions = Frame("x" -> Series("a" -> "aaa","b" -> "bbb")),
              columns = 2
            )
          )
          .sequences(
            "sequence viewer",
            List(
              "name1|dfsf|dfsd" -> "DIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNG",
              "name2" -> "DIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNGDIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNGDIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNGDIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNGDIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNGDIFNSKYDCKIMTSKTDISSSVITSLGAIVSCYGKTKCTASNKNRGIIKTFSNG"
            )
          )
          .graph(
            "graph",
            Frame(
              Series("a" -> 1d, "b" -> 2d, "c" -> 0d),
              Series("a" -> 1d, "b" -> 2d, "c" -> 3d),
              Series("a" -> 1d, "b" -> 2d, "c" -> 3d)
            ),
            Mat(Vec(0, 1, 0.5), Vec(1, 2, 0.5)).T,
            Vec(0, 0, 1),
            Seq(0 -> "0", 1 -> "1"),
            Series(
              0 -> "hello A dfs af sdfa faf sfd",
              2 -> "C dsfa ff af safdsds f"
            ),
            width = 1000
          )
          .graph(
            "graph",
            Frame(
              Series("a" -> 1d, "b" -> 2d, "c" -> 3d),
              Series("a" -> 1d, "b" -> 2d, "c" -> 3d)
            ),
            Mat(Vec(0, 1, 0.5), Vec(1, 2, 0.5)).T,
            Vec(0, 0, 1),
            Seq(0 -> "0", 1 -> "1"),
            Series(
              0 -> "hello A dfs af sdfa faf sfd",
              2 -> "C dsfa ff af safdsds f"
            ),
            width = 2000
          )
          .frame(
            "frame",
            Frame(
              "abc" -> Series(1 to 999 map (i => i.toString -> i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*),
              "def" -> Series(1 to 999 map (i => i.toString -> -i): _*)
            ),
            highlights = Some(-10d, 10d)
          )
      )
      .stringFrame(
        "frame",
        Frame(
          "abc" -> Series(1 to 999 map (i => i.toString -> i.toString): _*),
          "def" -> Series(1 to 999 map (i => i.toString -> (-i).toString): _*)
        ),
        ignoreRowIx = true
      )
      .stringFrame(
        "frame",
        Frame(
          ("abc",1) -> Series(1 to 999 map (i => (i.toString,i) -> i.toString): _*),
          ("def",1) -> Series(1 to 999 map (i => (i.toString,i) -> (-i).toString): _*)
        ),
        rowIxHeader = Some(List("A","B"))
      )
      .stringFrame(
        "frame",
        Frame(
          ("abc",1) -> Series(1 to 999 map (i => (i.toString,i) -> i.toString): _*),
          ("def",1) -> Series(1 to 999 map (i => (i.toString,i) -> (-i).toString): _*)
        ),
      )
      .stringFrame(
        "frame",
        Frame(
          ("abc",1) -> Series(1 to 999 map (i => i.toString -> i.toString): _*),
          ("def",1) -> Series(1 to 999 map (i => i.toString -> (-i).toString): _*)
        ),
      )
      .plot("blah", plot, width = 3000, pdf =true)
      .plot("blah", plot, cellWidth = Some(200))
      .footer("footer")
      .plot("blah svg", plot, svg = true, pdf = true)
      .data("data1", "Hi There!")
      .libraryCode("""
      function upper(whatever) {
        return whatever.toUpperCase()
      }
      """)
      .javascript("JS", { id =>
        s"""
        document.getElementById('$id').appendChild(document.createTextNode(upper(getGlobalData('data1'))));
        """
      })
      .withSource(classpathBinary("/sources.jar"))
      .render
    val testhtml = "test.html"
    val fw = new java.io.FileWriter(new java.io.File(testhtml))
    fw.write(html)
    fw.close
    println(new java.io.File(testhtml).getAbsolutePath)
  }

}
