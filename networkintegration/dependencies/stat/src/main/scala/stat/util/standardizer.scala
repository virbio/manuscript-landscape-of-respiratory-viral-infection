package stat.util

import org.saddle._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import stat.Transform1
import stat.TrainTransform1

case class Standardizer[RX: ST: ORD, CX: ST: ORD](
    sd: Vec[Double],
    mean: Vec[Double]
) extends Transform1[Frame[RX, CX, Double], Standardizer[RX, CX]] {
  def transform(f: Frame[RX, CX, Double]) = Standardizer.transform(f, sd, mean)
}

object Standardizer {
  def transform[RX: ST: ORD, CX: ST: ORD](
      f: Frame[RX, CX, Double],
      mean: Vec[Double],
      sd: Vec[Double]
  ) = {
    val transformedColumns = f.toColSeq.zipWithIndex.map {
      case ((cx, col), idx) =>
        (col.toVec - mean.raw(idx)) / sd.raw(idx)
    }
    Frame(transformedColumns, f.rowIx, f.colIx)
  }
  def apply[RX: ST: ORD, CX: ST: ORD](
      f: Frame[RX, CX, Double]
  ): Standardizer[RX, CX] = {
    val mean = f.reduce(_.toVec.mean2).toVec
    val sd = f.reduce(_.toVec.sampleStandardDeviation).toVec
    Standardizer(mean, sd)
  }
  def trainer[RX: ST: ORD, CX: ST: ORD] = new StandardizerTrainer[RX, CX]
}

class StandardizerTrainer[RX: ST: ORD, CX: ST: ORD]
    extends TrainTransform1[Frame[RX, CX, Double], Standardizer[RX, CX]] {
  def train(i: Frame[RX, CX, Double]) = Standardizer.apply(i)
}
