package vir.networkintegration.notebook

import vir.networkintegration.cli.DataConfig
import vir.networkintegration.rwr
import vir.networkintegration._
import stories._
import org.saddle._
import org.saddle.order._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import org.saddle.scalar.ScalarTagDouble
import org.saddle.index.OuterJoin
import org.nspl._
import org.nspl.saddle._
import org.nspl.awtrenderer._
import org.saddle.index.InnerJoin
import tasks.TaskSystemComponents
import org.nspl.Shapes
// import vir.networkintegration.ranking.await
import vir.networkintegration.ranking.tasks.MashupInput
import org.saddle.index.RightJoin
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import cats.effect.IO
import cats.effect.unsafe.implicits.global

/** Smoothes and integrates per-pathogen screens over the reactome graph guided by subpool data
  */
object Notebook9 {

  def await[T](f: IO[T]): T = {
    f.unsafeRunSync
  }

  def highestSignificantPValueByFDR(Q: Double, pValues: Iterable[Double]): Double = {
    val Qpm = Q / pValues.size.toDouble
    pValues.toSeq.sorted.zipWithIndex.reverse.find(x => x._1 <= (x._2 + 1).toDouble * Qpm).map(_._1) match {
      case Some(x) => x
      case None    => 0.0f
    }
  }

  def writeGraphToZip[T <: Renderable[T]](
      name: String,
      graph: Graph,
      file: String,
      nodeAnnotations: Frame[String, String, String],
      geneOrigin: Seq[(String, Seq[(String, String)])],
      reactomeGenes: Index[String],
      umapPlot: T
  )(implicit re: Renderer[T, JavaRC]) = {
    import _root_.io.circe.syntax._

    val zout = new ZipOutputStream(new FileOutputStream(file));
    zout.putNextEntry(new ZipEntry(s"$name.nodelocations.csv"))
    org.saddle.csv.CsvWriter
      .writeFrameToStream(graph.nodeLocations, zout)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry(s"$name.edges.csv"))
    org.saddle.csv.CsvWriter
      .writeFrameToStream(graph.edges.toFrame, zout)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry(s"$name.nodeannotations.csv"))
    org.saddle.csv.CsvWriter
      .writeFrameToStream(
        nodeAnnotations.reindexRow(graph.nodeLocations.rowIx),
        zout
      )
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry(s"$name.geneorigin.json"))

    zout.write(
      geneOrigin.asJson.noSpaces
        .getBytes("UTF-8")
    )
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry(s"$name.pathogen-counts.summary.json"))

    zout.write(
      nodeAnnotations
        .firstCol("pathogen-count")
        .toSeq
        .groupBy(_._2)
        .toSeq
        .map(v => (v._1, v._2.size))
        .asJson
        .noSpaces
        .getBytes("UTF-8")
    )
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry(s"$name.reactome.txt"))

    zout.write(
      reactomeGenes.toSeq.mkString("\n").getBytes("UTF-8")
    )
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry(s"$name.network.plot.pdf"))
    zout.write(pdfToByteArray(umapPlot))
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry(s"$name.network.plot.png"))
    zout.write(pngToByteArray(umapPlot, width = 5000))
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry(s"$name.network.plot.svg"))
    zout.write(svgToByteArray(umapPlot, width = 5000))
    zout.closeEntry()

    zout.close()

  }

  def fisher(ps: Vec[Double]) = {
    val k = ps.count
    val teststat = ps.map(math.log).sum * -2.0
    jdistlib.ChiSquare.cumulative(teststat, 2 * k, false, false)
  }

  assert(fisher(Vec(0.9)) == 0.9)
  assert(fisher(Vec(0.9, 0.9)) == 0.9806840353656787)
  assert(fisher(Vec(0.9, 0.9, Double.NaN)) == 0.9806840353656787)
  assert(fisher(Vec(0.5, 1e-10)) == 1.235949905525023e-9)

  def readMageck(
      file: String,
      pathogenName: String,
      mouseMapping: Map[String, String],
      agmMapping: Map[String, String]
  ) = {
    val mageckTable =
      Notebook1.parseMageck(file, mouseMapping, agmMapping)

    val proPathogenComparison = true

    val propathogenPValues =
      mageckTable.firstCol("pos_p_value")

    val antipathogenPValues =
      mageckTable.firstCol("neg_p_value")

    val fileName = new java.io.File(file).getName.split("_").head

    val minP =
      propathogenPValues.joinMap(antipathogenPValues, OuterJoin)((a, b) =>
        if (a.isNaN) b
        else if (b.isNaN) a
        else
          math.min(a, b)
      )
    List(
      (
        s"${pathogenName}_" + fileName,
        minP
          .mapValues(d => -math.log10(d))
          .groupBy
          .combine(_.max2)
      )
    )
  }
  def readMageckProAnti(
      file: String,
      pathogenName: String,
      data: DataConfig
  ) = {
    val mouseMapping = Notebook1.parseMouseMapping(data.mouseMapping)
    val agmMapping = Notebook1.parseMouseMapping(data.agmMapping)
    val mageckTable =
      Notebook1.parseMageck(file, mouseMapping, agmMapping)

    val propathogenPValues =
      mageckTable.firstCol("pos_p_value")
    val antipathogenPValues =
       mageckTable.firstCol("neg_p_value")
      
    val fileName = new java.io.File(file).getName.split("_").head

    List(
      (
        pathogenName,
        propathogenPValues.groupBy
          .combine(_.min2),
        antipathogenPValues.groupBy
          .combine(_.min2)
      )
    )
  }

  def create(data: DataConfig)(
      implicit ts: TaskSystemComponents
  ) = {
    val pThreshold = 1e-4

    val ppiEdges =
      data.ppiGMT.flatMap(Notebook1.parseGMT).map(v => (v, 1d)).toSeries

    val reactomeGenes = Notebook1
      .parseReactome(data.reactomefi)
      .index
      .toSeq
      .flatMap(v => List(v._1, v._2))
      .distinct
      .toIndex

    val edgeList = Notebook1
      .parseReactome(data.reactomefi)
      .mapValues { f =>
        val d = f.toDouble
        if (d > 0d) 1d else 0d
      } concat ppiEdges

    val mashupFeatures = {
      val sf = await(
        ranking.tasks.MashupTask
          .task(
            MashupInput(
              List(edgeList),
              rwr.Hyperparameter(
                iterations = 15,
                restartProbability = 0.5,
                degreeNormalization = rwr.NoDegreeNormalization
              ),
              numberOfComponents = 10
            )
          )(tasks.ResourceRequest((16), 20000, 1))
      )
      sf
    }

    val mashupFrame =
      mashupFeatures.file.use(file => IO(Notebook1.readBinaryFrameFromFile(file))).unsafeRunSync

    val mouseMapping = Notebook1.parseMouseMapping(data.mouseMapping)
    val agmMapping = Notebook1.parseMouseMapping(data.agmMapping)

    val fluCrisprInputs = data.fluCrisprs.flatMap { s => readMageck(s, "flu", mouseMapping, agmMapping) }
    val hrvCrisprInputs = data.hrvCrisprs.flatMap { s => readMageck(s, "hrv", mouseMapping, agmMapping) }
    val rsvCrisprInputs = data.rsvCrisprs.flatMap { s => readMageck(s, "rsv", mouseMapping, agmMapping) }
    val piv3CrisprInputs = data.piv3Crisprs.flatMap { s => readMageck(s, "piv3", mouseMapping, agmMapping) }
    val hucovNL63CrisprInputs = data.hucovNL63Crisprs.flatMap { s =>
      readMageck(s, "hucovNL63", mouseMapping, agmMapping)
    }
    val hucovOC43CrisprInputs = data.hucovOC43Crisprs.flatMap { s =>
      readMageck(s, "hucovOC43", mouseMapping, agmMapping)
    }
    val hucov229ECrisprInputs = data.hucov229ECrisprs.flatMap { s =>
      readMageck(s, "hucov229E", mouseMapping, agmMapping)
    }
    val sars2CrisprInputs = data.sars2Crisprs.flatMap { s => readMageck(s, "sars2", mouseMapping, agmMapping) }
    val mersCrisprInputs = data.mersCrisprs.flatMap { s => readMageck(s, "mers", mouseMapping, agmMapping) }

    val imputablePathogens = List(
      "flu" -> fluCrisprInputs,
      "hrv" -> hrvCrisprInputs,
      "rsv" -> rsvCrisprInputs,
      "piv3" -> piv3CrisprInputs,
      "hucovNL63" -> hucovNL63CrisprInputs,
      "hucovOC43" -> hucovOC43CrisprInputs,
      "hucov229E" -> hucov229ECrisprInputs,
      "sars2" -> sars2CrisprInputs,
      "mers" -> mersCrisprInputs
    ).filter(_._2.nonEmpty)

    val genomewideAggregatedPValuesPerPathogen = imputablePathogens.map {
      case (
          pathogen,
          genomewideScreens: Seq[(String, Series[String, Double])]
          ) =>
        val withSubpool = genomewideScreens

        val combinedPValue = withSubpool.toFrame
          .rreduce(s => fisher(s.toVec.dropNA.map(x => math.pow(10, -x))))
          .sorted
          .dropNA

        (pathogen, combinedPValue)
    }
    println(s"Genes per pathogen: ${genomewideAggregatedPValuesPerPathogen
      .map(v => v._1 -> v._2.dropNA.length)}")

    val genomewideAggregatedPValuesPerPathogenFrame = {
      val index = genomewideAggregatedPValuesPerPathogen
        .map(_._2.index)
        .reduce((a, b) => a.join(b, InnerJoin).index.distinct)
      genomewideAggregatedPValuesPerPathogen
        .map(s => (s._1, s._2.distinctIx.reindex(index)))
        .toFrame
    }

    val (
      propathogenAggregatedPerPathogenPValues,
      _
    ) = {

      val pvalues =
        data.fluCrisprs.flatMap(readMageckProAnti(_, "flu", data)) ++
          data.hrvCrisprs.flatMap(readMageckProAnti(_, "hrv", data)) ++
          data.rsvCrisprs.flatMap(readMageckProAnti(_, "rsv", data)) ++
          data.piv3Crisprs.flatMap(readMageckProAnti(_, "piv3", data)) ++
          data.hucovNL63Crisprs
            .flatMap(readMageckProAnti(_, "hucovNL63", data)) ++
          data.hucovOC43Crisprs
            .flatMap(readMageckProAnti(_, "hucovOC43", data)) ++
          data.hucov229ECrisprs.flatMap(readMageckProAnti(_, "hucov229E", data)) ++
          data.sars2Crisprs.flatMap(readMageckProAnti(_, "sars2", data)) ++
          data.mersCrisprs.flatMap(readMageckProAnti(_, "mers", data))

      val pro = Frame(
        pvalues.map(v => v._1 -> v._2): _*
      )
      val anti = Frame(
        pvalues.map(v => v._1 -> v._3): _*
      )

      val proAggr = pro.T.groupBy
        .combine(v => fisher(v.dropNA))
        .T
        .row(genomewideAggregatedPValuesPerPathogenFrame.rowIx.toVec.toArray)
      val antiAggr = anti.T.groupBy
        .combine(v => fisher(v.dropNA))
        .T
        .row(genomewideAggregatedPValuesPerPathogenFrame.rowIx.toVec.toArray)

      (proAggr, antiAggr)

    }

    val genesToHighlight = List(
      "STT3A",
      "STT3B",
      "DOHH",
      "DHPS",
      "EIF5A"
    )

    // https://www.medrxiv.org/content/10.1101/2020.12.14.20248176v3.full.pdf
    //https://www.nature.com/articles/s41586-021-03767-x
    val covid19GWASHorowitzAndHGI =
      List(
        "LZTFL1",
        "SLC6A20",
        "MHC",
        "ABO",
        "DPP9",
        "IFNAR2",
        "RPL24",
        "FOXP4",
        "NXPE3",
        "SENP7",
        "TCF19",
        "TCL19", // one of them is a typo in the preprint?
        "LST1",
        "HLA-C",
        "IL10RB",
        "C6orf15",
        "CCHCR1",
        "PSORS1C1",
        "TMEM65",
        "OAS1",
        "KANSL1",
        "TAC4",
        "RAVER1",
        "PLEKHA4"
      )

    val casanovaGeneList =
      List("IRF7", "IRF9", "TLR3", "MX1", "UN93B1", "TRIF", "TBK1", "IRF3", "IFNAR2", "TLR7", "IFIH1")

    val fdrCutoff = highestSignificantPValueByFDR(0.05, genomewideAggregatedPValuesPerPathogenFrame.toMat.toVec.toSeq)

    val pValueThresholdInProfiles = fdrCutoff

    val obligatoryGenesInClusters =
      (
        genomewideAggregatedPValuesPerPathogenFrame
          .rfilter(_.countif(_ < pValueThresholdInProfiles) > 0)
          .rowIx
          .toSeq
      ) ++ genesToHighlight ++ covid19GWASHorowitzAndHGI ++ casanovaGeneList

    def makeClusters(
        profileFrameShortAndLongWithGeneOrigin: (
            (
                Frame[
                  String,
                  String,
                  Double
                ],
                Frame[String, String, Double]
            ),
            Seq[(String, Seq[(String, String)])]
        )
    ) = {
      val (profileFrameShortAndLong, geneOrigin) =
        profileFrameShortAndLongWithGeneOrigin
      val profileFrame = profileFrameShortAndLong._1

      val normalizedProfileFrame = profileFrame.mapVec { col =>
        val mean = col.mean2
        val std = col.sampleStandardDeviation
        (col - mean) / std
      }


      implicit val sparseMatOps = stat.sparse.SparseMatOps


      val geneOriginMap = geneOrigin.toMap

      def labelsFromGeneOrigins(
          labelTypes: Seq[String],
          filterValues: Seq[String]
      ) = {
        val labelList = profileFrame.rowIx.toSeq.map {
          case gene =>
            val label = geneOriginMap
              .get(gene)
              .getOrElse(Nil)
              .filter(a =>
                labelTypes
                  .contains(a._1) && filterValues.isEmpty || filterValues
                  .contains(a._2)
              )
              .map(v => if (labelTypes.size == 1) v._2 else v._1 + ":" + v._2)

            label
        }
        val labels = labelList.map(_.mkString(", "))
        val distinctLabels = labels.distinct.sorted
        val labelsInt = labels.map(distinctLabels.indexOf)
        val labelsByGene = (profileFrame.rowIx.toSeq zip labelList).toMap
        (distinctLabels, labelsInt, labelsByGene)
      }

      val (geneOriginPathogens, geneOriginPathogenGeneLabels, geneOriginPathogensGeneLabelListByGene) =
        labelsFromGeneOrigins(
          List("per-pathogen-aggregated-pvalue-less-than-threshold"),
          Nil
        )

      val pathogenCount = geneOriginPathogensGeneLabelListByGene.toSeq.toSeries.mapValues(_.size.toDouble)

      val clusterFrame = (Frame(
        "pathogen-count" -> pathogenCount
      ) rconcat profileFrame).sortedRowsBy(_.toVec.toSeq.head)

      val (umapLayout, umapEdges) = Umap
        .umapLamp(
          normalizedProfileFrame,
          dim = 2,
          device = lamp.CudaDevice(0)
        )

      val umapPlotProfileFrameColorByPathogenCount =
        Umap.plotWithColorScale(
          layout = umapLayout,
          edgeWeights = umapEdges,
          nodeWeights = pathogenCount,
          includeEdges = true,
          includeGeneNames = false,
          zeroClusterIsSmall = false,
          withoutZeroCluster = false,
          includeGeneNamesAboveNodeWeight = 3,
          includeGeneNamesSet = genesToHighlight.toSet
        )

      val graph = stories.Graph(
        nodeLocations = umapLayout,
        edges = umapEdges,
        nodeClasses = umapLayout.rowIx.toVec.map(g => pathogenCount.get(g).get.toInt),
        classLabels = Nil,
        extraTextByRowIdx = profileFrame.toRowSeq.zipWithIndex.map {
          case ((rowIx, series), idx) =>
            (idx, series.toSeq.map(p => p._1 + ": " + p._2).mkString(", "))
        } toSeries,
        edgeColors = Nil
      )

      val doc = Document()
        .frame(
          "table",
          clusterFrame,
          highlights = Some(0d -> 0d)
        )
        .svg(
          "plot by profile, color by pathogen",
          umapPlotProfileFrameColorByPathogenCount,
          drawTextAsCurves = false,
          pdf = true
        )
        .item(
          "browsable graph",
          graph = Some(graph),
          width = Some(4000)
        )

      (
        clusterFrame,
        graph,
        umapPlotProfileFrameColorByPathogenCount,
        geneOrigin,
        doc
      )

    }

    def selectGenesAcrossAllPathogensFromSmoothedOdds(
        perPathogenOdds: Seq[
          (String, Series[String, Double], Series[String, Double])
        ]
    ) = {

      def selectRows(
          pathogen: String,
          odds: Series[String, Double],
          pp: Series[String, Double]
      ) = {
        val f = odds.mapVec(_.rank(ascending = false))

        f.dropNA
          .filter { rank => rank <= 100 }

      }

      perPathogenOdds.flatMap {
        case (pathogen, odds, pp) =>
          selectRows(pathogen, odds, pp).index.toSeq
      }.toSeq

    }

    def makeProfileFromRawPValuesAndSmoothedOdds(
        perPathogenOdds: Seq[
          (String, Series[String, Double], Series[String, Double])
        ],
        concatenatedMashup: Boolean,
        noCrispr: Boolean
    ) = {

      val f0All = genomewideAggregatedPValuesPerPathogenFrame
        .mapVec(_.fillNA(_ => 1d))

      val genesWithLowPvalues = f0All.toColSeq.flatMap {
        case (pathogen, col) =>
          col.filter(_ < pValueThresholdInProfiles).index.toSeq.map { g =>
            (
              g,
              ("per-pathogen-aggregated-pvalue-less-than-threshold", pathogen)
            )
          }
      }

      val genesWithTopRanksInSmoothing =
        selectGenesAcrossAllPathogensFromSmoothedOdds(
          perPathogenOdds
        )

      val selectedGenes =
        (genesWithLowPvalues.map(_._1) ++ genesWithTopRanksInSmoothing ++ obligatoryGenesInClusters ++ List("STT3B")).distinct

      val profileFrameAllGenes = {
        val x = f0All
          .mapValues(d => -math.log10(if (d == 0) 1e-30 else d))
        if (noCrispr) mashupFrame.mapColIndex("mashup:" + _)
        else {
          if (concatenatedMashup)
            x.mapColIndex("pathogen-aggr--log10p:" + _)
              .rconcat(mashupFrame.mapColIndex("mashup:" + _), InnerJoin)
          else x.mapColIndex("pathogen-aggr--log10p:" + _)
        }
      }

      val profileFrameSelectedGenes =
        profileFrameAllGenes.row(selectedGenes.distinct: _*)

      val geneOrigin = (genesWithLowPvalues)
        .groupBy(_._1)
        .toSeq
        .map(v => (v._1, v._2.map(_._2)))

      (
        (profileFrameSelectedGenes, profileFrameAllGenes),
        geneOrigin
      )

    }

    val (evalFoldsWithoutSubpool, predictionsWithoutSubpool) =
      imputablePathogens.toList.map {
        case (pathogenName, _) =>
          val (_, aggregatedPvalue) =
            genomewideAggregatedPValuesPerPathogen
              .find(_._1 == pathogenName)
              .get

          val target = {

            val classes = aggregatedPvalue.map {
              case (name, pvalue) =>
                val cl =
                  if (pvalue < pThreshold) 1d
                  else 0d
                (name, cl)
            }
            classes
          }

          val (evaled, predicted) =
            await {
              ranking.tasks.SupervisedEvalAndImputeTask.task(
                ranking.tasks.SupervisedEvalAndImputeInput(
                  None,
                  mashupFeatures,
                  target
                )
              )(tasks.ResourceRequest(1, 10000, 1))
            }

          (
            evaled.zipWithIndex
              .map {
                case (series, idx) =>
                  (pathogenName + "-fold" + idx, series)
              },
            (pathogenName, (predicted, aggregatedPvalue))
          )
      }.unzip

    val perPathogenSummaryFramesWithoutSubpool = predictionsWithoutSubpool.map {
      case (pathogen, (pred, prePValue)) =>
        val score = pred.colAt(1).sorted.reversed
        val rank = score.mapVec(
          _.rank(ascending = false)
        )

        val odds = pred.rreduce { series =>
          val s1 = series.raw(1)
          val s2 = series.raw(0)
          val r = math.log10(s1 / s2)
          r
        }

        (
          pathogen,
          score,
          odds
        )

    }

    val (
      clusterFrameUnsmoothedCatMashup,
      clusterGraphUnsmoothedCatMashup,
      umapPlotUnsmoothedCatMashup,
      clusterGeneOriginsUnsmoothedCatMashup,
      clusterDocUnsmoothedCatMashup
    ) =
      makeClusters(
        makeProfileFromRawPValuesAndSmoothedOdds(
          perPathogenOdds = perPathogenSummaryFramesWithoutSubpool.map {
            case (pathogen, pp, odds) =>
              (pathogen, odds, pp)
          },
          concatenatedMashup = true,
          noCrispr = false
        )
      )
    val (
      clusterFrameUnsmoothed,
      clusterGraphUnsmoothed,
      umapPlotUnsmoothed,
      clusterGeneOriginsUnsmoothed,
      clusterDocUnsmoothed
    ) =
      makeClusters(
        makeProfileFromRawPValuesAndSmoothedOdds(
          perPathogenOdds = perPathogenSummaryFramesWithoutSubpool.map {
            case (pathogen, pp, odds) =>
              (pathogen, odds, pp)
          },
          concatenatedMashup = false,
          noCrispr = false
        )
      )
    val (
      clusterFrameNoCrispr,
      clusterGraphNoCrispr,
      umapPlotNoCrispr,
      clusterGeneOriginsNoCrispr,
      clusterDocNoCrispr
    ) =
      makeClusters(
        makeProfileFromRawPValuesAndSmoothedOdds(
          perPathogenOdds = perPathogenSummaryFramesWithoutSubpool.map {
            case (pathogen, pp, odds) =>
              (pathogen, odds, pp)
          },
          concatenatedMashup = false,
          noCrispr = true
        )
      )

    println("cluster plots done")

    println("done")

    val exportedGraphs = Map(
      (
        "unsmoothed",
        (
          clusterGraphUnsmoothed,
          clusterFrameUnsmoothed
            .mapValues(_.toString),
          clusterGeneOriginsUnsmoothed,
          reactomeGenes,
          umapPlotUnsmoothed
        )
      ),
      (
        "unsmoothed_concat_mashup",
        (
          clusterGraphUnsmoothedCatMashup,
          clusterFrameUnsmoothedCatMashup
            .mapValues(_.toString),
          clusterGeneOriginsUnsmoothedCatMashup,
          reactomeGenes,
          umapPlotUnsmoothedCatMashup
        )
      ),
      (
        "nocrispr",
        (
          clusterGraphNoCrispr,
          clusterFrameNoCrispr
            .mapValues(_.toString),
          clusterGeneOriginsNoCrispr,
          reactomeGenes,
          umapPlotNoCrispr
        )
      )
    )

    val longDocument = Document()
      .pageTitle("CRISPR profiles")
      .section("Unsmoothed crispr (no reactome)")
      .document(clusterDocUnsmoothed)
      .section("Unsmoothed cat. reactome")
      .document(clusterDocUnsmoothedCatMashup)
      .section("Just reactome")
      .document(clusterDocNoCrispr)

    (longDocument, exportedGraphs)
  }

}
