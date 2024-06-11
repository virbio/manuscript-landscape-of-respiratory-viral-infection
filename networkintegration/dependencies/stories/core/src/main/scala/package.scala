package object stories {
  def classpath(p: String) = {
    val s = scala.io.Source.fromInputStream(getClass.getResourceAsStream(p))
    try {
      s.mkString
    } finally {
      s.close
    }
  }
  def classpathBinary(p: String) = {
    val s = getClass.getResourceAsStream(p)
    try {
      val bArray =
        Stream.continually(s.read).takeWhile(-1 !=).map(_.toByte).toArray
      bArray
    } finally {
      s.close
    }
  }

  def markdown2html(source: String) = {
    import com.vladsch.flexmark.html.HtmlRenderer;
    import com.vladsch.flexmark.parser.Parser;
    import com.vladsch.flexmark.util.data.MutableDataSet;
    val options = new MutableDataSet();

    // uncomment to set optional extensions
    //options.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create(), StrikethroughExtension.create()));

    // uncomment to convert soft-breaks to hard breaks
    //options.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

    val parser = Parser.builder(options).build();
    val renderer = HtmlRenderer.builder(options).build();

    // You can re-use parser and renderer instances
    val document = parser.parse(source);
    val html = renderer.render(document); // "<p>This is <em>Sparta</em></p>\n"
    html
  }
}
