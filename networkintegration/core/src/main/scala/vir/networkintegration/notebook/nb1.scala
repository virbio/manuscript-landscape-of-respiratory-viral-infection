package vir.networkintegration.notebook
import vir.networkintegration.cli.DataConfig
import stories._
import org.saddle._
import org.saddle.order._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import org.saddle.scalar.ScalarTagDouble
import org.saddle.index.OuterJoin
import org.nspl._
import org.nspl.awtrenderer._
import org.saddle.index.InnerJoin
import java.io.File

object Notebook1 {

    def readBinaryFrameFromFile(file: File) = {
    val is = new java.io.FileInputStream(file)
    val readableChannel = is.getChannel
    val table =
      org.saddle.binary.Reader
        .readFrameFromChannel[Double](readableChannel)
        .right
        .get
    readableChannel.close
    table
  }
  
  

  def writeBinaryFrameToFile(
      file: File,
      frame: Frame[String, String, Double]
  ) = {
    val os = new java.io.FileOutputStream(file)
    val writableChannel = os.getChannel
    org.saddle.binary.Writer
      .writeFrameIntoChannel(frame, writableChannel)
      .right
      .get
    writableChannel.close

  }
 

  def parseReactome(file: String) = {
    org.saddle.csv.CsvParser
      .parseFileWithHeader[String](
        new java.io.File(file),
        fieldSeparator = '\t',
        recordSeparator = "\n",
        cols = List(0, 1, 4)
      )
      .right
      .get
      .withRowIndex(0, 1)
      .firstCol("Score")
  }

  def parseGMT(file: String) =
    fileutils
      .openSource(file)(_.getLines.toList)
      .flatMap { line =>
        val spl = line.split("\\t")
        val gene1 = spl(0)
        spl.drop(2).map { g => (gene1, g) }
      }
      .toList

  def parseMouseMapping(file: String): Map[String, String] =
    fileutils
      .openSource(file)(_.getLines.drop(1).toList)
      .map { line =>
        val spl = line.split("\\t")
        val mouse = spl(0)
        val human = spl(1)
        (mouse, human)
      }
      .filter(s => s._1.nonEmpty && s._2.nonEmpty)
      .toMap

  def parseMageck(file: String, mouseMapping: Map[String, String], agmMapping: Map[String, String]) = {

    org.saddle.csv.CsvParser
      .parseFileWithHeader[String](
        new java.io.File(file),
        fieldSeparator = '\t',
        recordSeparator = "\n"
      )
      .right
      .get
      .withRowIndex(0)
      .mapRowIndex { s => mouseMapping.get(s).orElse(agmMapping.get(s)).getOrElse(s) }
      .col("neg_p_value", "pos_p_value", "pos_lfc", "pos_fdr", "neg_fdr")
      .mapValues(s => ScalarTagDouble.parse(s.toCharArray(), 0, s.length))
  }

}
