package vir.networkintegration.cli
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config
import scopt.OParser
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import scala.collection.JavaConverters._
import java.io.File
import tasks._
import vir.networkintegration.notebook.Notebook9
import org.saddle.index.LeftJoin

case class DataConfig(
    reactomefi: String,
    fluCrisprs: Seq[String],
    hrvCrisprs: Seq[String],
    rsvCrisprs: Seq[String],
    piv3Crisprs: Seq[String],
    hucovNL63Crisprs: Seq[String],
    hucovOC43Crisprs: Seq[String],
    hucov229ECrisprs: Seq[String],
    sars2Crisprs: Seq[String],
    mersCrisprs: Seq[String],
    ppiGMT: Seq[String],
    mouseMapping: String,
    agmMapping: String,
) {
  def json =
    this.asJson

}
object DataConfig {
  def fromConfig(config: Config): DataConfig = {
    DataConfig(
      reactomefi = config.getString("reactomefi"),
      fluCrisprs = config.getStringList("fluCrispr").asScala.toSeq,
      hrvCrisprs = config.getStringList("hrvCrispr").asScala.toSeq,
      rsvCrisprs = config.getStringList("rsvCrispr").asScala.toSeq,
      piv3Crisprs = config.getStringList("piv3Crispr").asScala.toSeq,
      hucovNL63Crisprs = config.getStringList("hucovNL63Crispr").asScala.toSeq,
      hucovOC43Crisprs = config.getStringList("hucovOC43Crispr").asScala.toSeq,
      hucov229ECrisprs = config.getStringList("hucov229ECrispr").asScala.toSeq,
      sars2Crisprs = config.getStringList("sars2Crispr").asScala.toSeq,
      mersCrisprs = config.getStringList("mersCrispr").asScala.toSeq,
      ppiGMT = config.getStringList("ppiGMT").asScala.toSeq,
      mouseMapping = config.getString("mouseMapping"),
      agmMapping = config.getString("agmMapping"),
     
    )
  }
}

object CliParser {
  case class CliConfig(
      config: String = "",
      notebooks: String = "./",
      tempdir: String = "./",
  ) {
    def json = {
      this.asJson
    }
  }
  val builder = OParser.builder[CliConfig]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("hdt-network"),
      opt[String]("config")
        .action((p, c) => c.copy(config = p)),
      opt[String]("temp-dir")
        .action((p, c) => c.copy(tempdir = p)),
      opt[String]("out-nb")
        .action((p, c) => c.copy(notebooks = p)),
    )
  }
}

object Cli extends App {

  import CliParser._

  OParser.parse(CliParser.parser1, args, CliConfig()) match {
    case Some(cliConfig) =>
      val cfg = ConfigFactory
        .parseFile(new java.io.File(cliConfig.config))
        .withFallback(ConfigFactory.load())

      val dataConfig =
        DataConfig.fromConfig(cfg.getConfig("vir.networkintegration"))

      val header = s"""|START HDT NETWORK INTEGRATION ANALYSIS
      |Date: ${java.time.ZonedDateTime.now}
      |$$PWD: ${System.getenv("PWD")}
      |Command line: ${cliConfig.json.noSpaces}
      |Data configuration: ${dataConfig.json.noSpaces}
      |""".stripMargin


      withTaskSystem(ConfigFactory.parseString(s"""
      hosts.RAM = 20000
      hosts.numCPU = 16
      tasks.fileservice.storageURI = "${cliConfig.tempdir}"
      tasks.disableRemoting = true
      akka.loglevel=OFF
      """)) { implicit ts =>

        val nb9 =
            List(
              vir.networkintegration.notebook.Notebook9
                .create(
                  dataConfig
                )
            )

        val document =
          (nb9
            .map(_._1))
            .foldLeft(
              stories
                .Document()
            )(_ ++ _)
            .addAsPage(
              "meta",
              stories
                .Document()
                .section("Appendix")
                .git(buildinfo.BuildInfo.version)
                .withSource(stories.classpathBinary("/sources.jar"))
            )

        val file =
          s"${cliConfig.notebooks}${document.instant}cp_data_${document.instant}.html"
        if (nb9.nonEmpty) {


          nb9
            .flatMap(_._2.toList)
            .map {
              case (
                  name,
                  (graph, annotation, geneOrigin, reactome, umapPlot)
                  ) =>
                val path =
                  s"${cliConfig.notebooks}${document.instant}graphdata_${name}.zip"
                import org.nspl.awtrenderer._
                Notebook9.writeGraphToZip(
                  name,
                  graph,
                  path,
                  annotation,
                  geneOrigin,
                  reactome,
                  umapPlot
                )
                path
            }
            .foreach(println)
        }

        fileutils.writeToFile(new java.io.File(file), document.render)

        println(file)

        println("DONE @ " + java.time.ZonedDateTime.now)
      }
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
