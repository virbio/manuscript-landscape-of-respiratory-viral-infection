package vir.networkintegration

import org.saddle._
import org.saddle.order._
import org.saddle.index.InnerJoin
import smile.classification.{RandomForest => ClassificationRF}
import smile.regression.{RandomForest => RegressionRF}
import stat.util.RemoveColumns

package object rf {

  def trainingMethod(numClasses: Int) =
    (
        counts: Mat[Double],
        status: Mat[Double]
    ) => {
      val asFrame =
        counts.toFrame.mapColIndex(_.toString).mapRowIndex(_.toString)
      val transformation = RemoveColumns
        .removeConstantsTrainer[String, String]
        .train(asFrame)

      val trained = trainRF(
        transformation.transform(asFrame),
        Series(status.col(0)).mapIndex(_.toString)
      )
      val fun = {
        (
          evalCount: Mat[Double]
        ) =>
          val asFrame =
            evalCount.toFrame.mapColIndex(_.toString).mapRowIndex(_.toString)
          val validationX =
            transformation.transform(
              asFrame
            )

          val predictionScores = predictRF(trained, validationX, numClasses)
          assert(predictionScores.rowIx == asFrame.rowIx)
          predictionScores.toMat

      }

      (fun,())
    }

  private def trainRF(
      counts: Frame[String, String, Double],
      status: Series[String, Double],
      trees: Int = 500
  ) = {
    import smile.data.vector.BaseVector
    import smile.data.vector.IntVector
    import smile.classification.RandomForest
    val data1 = counts
      .addCol(status, "status", InnerJoin)

    val data2 = data1
      .filterIx(_ != "status")
      .toRowSeq
      .map(_._2.toVec.toArray)
      .toArray
    val props = (new java.util.Properties)
    props
      .setProperty(
        "smile.random.forest.mtry",
        math.sqrt(data1.numCols).toInt.toString
      )
    props.setProperty("smile.random.forest.trees", trees.toString)

    try {
      RandomForest.fit(
        smile.data.formula.Formula
          .lhs("status"),
        smile.data.DataFrame
          .of(
            data2,
            data1.colIx.toSeq.toArray.filter(_ != "status"): _*
          )
          .merge(
            IntVector.of(
              "status",
              data1.firstCol("status").toVec.toArray.map(_.toInt)
            )
          ),
        props
      )
    } catch {
      case e: Exception =>
        println("RF error on: " + data1)
        throw e
    }
  }
  
  private def predictRF(
      rf: ClassificationRF,
      counts: Frame[String, String, Double],
      numClasses: Int = 2
  ) = {
    val smileDF = {
      smile.data.DataFrame.of(
        counts.toMat.rows.map(_.toArray).toArray,
        counts.colIx.toSeq.toArray: _*
      )
    }

    counts.rowIx.toSeq.zipWithIndex
      .map {
        case (_, idx) =>
          val scores = Array.ofDim[Double](numClasses)
          rf.predict(smileDF.get(idx), scores)
          scores.toVec
      }
      .toMat
      .T
      .toFrame
      .setRowIndex(counts.rowIx)
  }
}
