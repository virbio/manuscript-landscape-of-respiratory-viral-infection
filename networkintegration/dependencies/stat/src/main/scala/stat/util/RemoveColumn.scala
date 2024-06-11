package stat.util

import org.saddle._
import org.saddle.linalg._
import stat.Transform1
import stat.TrainTransform1

case class RemoveColumns[RX, CX](remove: Set[CX])
    extends Transform1[
      Frame[RX, CX, Double],
      RemoveColumns[RX, CX]
    ] {
  def transform(f: Frame[RX, CX, Double]) =
    f.filterIx(ix => !remove.contains(ix))
}
case class RemoveColumnsM(remove: Array[Int])
    extends Transform1[
      Mat[Double],
      RemoveColumnsM
    ] {
  def transform(f: Mat[Double]) = {
    f.withoutCols(remove)
  }
}

object RemoveColumns {
  def removeConstantM(
      f: Mat[Double]
  ): RemoveColumnsM = {
    RemoveColumnsM(
      f.cols.zipWithIndex
        .filter {
          case (vec, idx) =>
            val sd = vec.sampleStandardDeviation
            sd.isNaN || sd == 0d
        }
        .map(_._2)
        .toArray
    )
  }
  def removeConstant[RX, CX](
      f: Frame[RX, CX, Double]
  ): RemoveColumns[RX, CX] = {
    RemoveColumns[RX, CX](
      f.toColSeq
        .filter {
          case (cix, series) =>
            val sd = series.toVec.sampleStandardDeviation
            sd.isNaN || sd == 0d
        }
        .map(_._1)
        .toSet
    )
  }
  def removeColumns[RX, CX](f: Frame[RX, CX, Double])(
      fun: Frame[RX, CX, Double] => Set[CX]
  ): RemoveColumns[RX, CX] = RemoveColumns(fun(f))
  def removeConstantsTrainer[RX, CX] = new RemoveConstants[RX, CX]
  def removeConstantsTrainerM = new RemoveConstantsM
  def removeColumnsTrainer[RX, CX](fun: Frame[RX, CX, Double] => Set[CX]) =
    new RemoveColumnsTrainer[RX, CX](fun)
}
class RemoveConstants[RX, CX]
    extends TrainTransform1[Frame[RX, CX, Double], RemoveColumns[RX, CX]] {
  def train(f: Frame[RX, CX, Double]) = RemoveColumns.removeConstant(f)

}
class RemoveConstantsM extends TrainTransform1[Mat[Double], RemoveColumnsM] {
  def train(f: Mat[Double]) = RemoveColumns.removeConstantM(f)

}
class RemoveColumnsTrainer[RX, CX](fun: Frame[RX, CX, Double] => Set[CX])
    extends TrainTransform1[Frame[RX, CX, Double], RemoveColumns[RX, CX]] {
  def train(f: Frame[RX, CX, Double]) = RemoveColumns.removeColumns(f)(fun)

}
