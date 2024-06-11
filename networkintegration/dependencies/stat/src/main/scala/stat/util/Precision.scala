package stat.util

import org.saddle._
import org.saddle.order._

object Precision {

  /** Compute precision values from sorted rankings
    *
    * @return an array with N+1 doubles.
    *   The i-th value is the precision of the first i+1 elements.
    */
  def fromSorted(
      scores: Array[Double],
      truth: Array[Boolean]
  ) = {
    var tp = 0d
    var fp = 0d
    val x = Array.ofDim[Double](scores.length)
    var scorePrev = Double.NaN
    var i = 0
    val N = scores.length
    while (i < N) {
      if (truth(i)) {
        tp += 1
      } else {
        fp += 1
      }
      if (scorePrev != scores(i)) {
        x(i) = tp / (tp + fp)
        scorePrev = scores(i)
      } else {
        x(i) = x(i - 1)
      }

      i += 1
    }
    x
  }

  def averagePrecisionFromSorted(
      scores: Array[Double],
      truth: Array[Boolean]
  ) = {
    import org.saddle._
    val prec = fromSorted(scores, truth)
    val locs = Vec(truth).find(identity)
    locs.map(l => prec(l)).mean
  }

  def averagePrecisionFromUnsorted(scores: Vec[Double], truth: Vec[Boolean]) = {
    val sort = array.argsort(scores).reverse

    averagePrecisionFromSorted(
      scores.take(sort).toArray,
      truth.take(sort).toArray
    )
  }
  def averagePrecisionFromUnsorted[T](
      scores: Series[T, Double],
      truth: Series[T, Boolean]
  ): Double =
    averagePrecisionFromUnsorted(
      scores.values,
      truth.reindex(scores.index).values
    )

}
