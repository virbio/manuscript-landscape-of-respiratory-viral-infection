package vir.networkintegration

import org.saddle._
import org.saddle.order._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import stat.cv.CVSplit
import vir.networkintegration.rwr._
import org.saddle.index.InnerJoin
import tasks.{ResourceRequest, TaskSystemComponents}
// import _root_.vir.networkintegration.ranking.tasks.ImputeInput
import scala.concurrent.Await
import scala.concurrent.Future
import _root_.vir.networkintegration.ranking.tasks.MashupInput
import org.saddle.index.LeftJoin
import org.saddle.index.OuterJoin
import stat.cv.KFold
import java.io.File
import vir.networkintegration.notebook.Notebook1
import stat.cv.SplitIdx
import scala.util.Random
import cats.effect.IO
import cats.effect.unsafe.implicits.global

package object ranking {

  def supervisedEvalAndImpute(
      crisprFeatures: Option[Frame[String, String, Double]],
      mashupFeatures: Frame[String, String, Double],
      target: Series[String, Double],
      rng: scala.util.Random
  ): (List[Series[String, Double]], Frame[String, Int, Double]) = {

    val features1 = crisprFeatures match {
      case Some(crisprFeatures) =>
        (crisprFeatures
          .mapColIndex(s => "crispr" + s)
          .rconcat(mashupFeatures))
          .rdropNA
      case None => mashupFeatures.rdropNA
    }

    val features = features1
      .row(target.index.toVec.toArray)
    val commonTarget = target.reindex(features.rowIx)
    val featureM = features.toMat
    val targetV = commonTarget.toVec
    val cvResult = stat.cv.EvalLoop
      .cv(
        featureM,
        Mat(targetV),
        List(
          targetV.find(_ == 0d),
          targetV.find(_ == 1d)
        ),
        stat.cv.KFold(3, rng, 1),
        predictable = List(features1.toMat)
      )(rf.trainingMethod(2), eval2Class)
    val predicteds = cvResult.map(_.predictionsOfPredictables.head)
    val evaled = cvResult.map(_.metric)

    val aggregatedPredictions = (predicteds
      .map { mat => mat.map(math.log) }
      .reduce(_ + _) / predicteds.size).map(math.exp)

    (
      evaled,
      aggregatedPredictions.toFrame.setRowIndex(
        features1.rowIx
      )
    )
  }

  val eval2Class = (predictionM: Mat[Double], targetM: Mat[Double]) => {
    val target = targetM.col(0)

    val ap1 = stat.util.Precision.averagePrecisionFromUnsorted(
      predictionM.col(1),
      target.map(_ == 1d)
    )
    val auc1VsElse = stat.util.ROC
      .aucFromUnsorted(
        predictionM.col(1),
        target.map(_ == 1d),
        1.0
      )
      .right
      .get
    val threshold1 = stat.util.ROC
      .thresholdFromUnsorted(
        predictionM.col(1),
        target.map(_ == 1d)
      )
      .right
      .get

    Series(
      "auc1" -> auc1VsElse,
      "ap1" -> ap1,
      "threshold1" -> threshold1,
      "evalcount0" -> target.countif(_ == 0d).toDouble,
      "evalcount1" -> target.countif(_ == 1d).toDouble
    )
  }

}
