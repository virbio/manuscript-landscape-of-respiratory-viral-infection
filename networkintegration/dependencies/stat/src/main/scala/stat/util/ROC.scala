package stat.util
import org.saddle._
import org.saddle.order._
object ROC {
  def curveFromUnsorted(scores: Vec[Double], truth: Vec[Boolean]) = {
    val sort = array.argsort(scores).reverse

    curveFromSorted(scores.take(sort).toArray, truth.take(sort).toArray)
  }

  /* Returns the corresponding score (first in tuple) and the maximal Youden index (second in tuple)
   */
  def maxYouden(
      scores: Vec[Double],
      truth: Vec[Boolean],
      maxFalsePositiveRate: Double
  ) = {
    youdenIndexFromUnsorted(scores, truth).map {
      case (fpr, _, scores, youden) =>
        // fpr is monotone increasing

        val i = array.argmax(
          youden.filterAt(idx => fpr(idx) <= maxFalsePositiveRate).toArray
        )
        (scores(i), youden(i))
    }
  }
  /* The vertical differene over/under the diagonal line
   * J = sensitivity + specificity - 1
   * returns for arrays: (x=false positive rate (1-specificity),y=sensitivity=recall,scores,J)
   */
  def youdenIndexFromUnsorted(scores: Vec[Double], truth: Vec[Boolean]) = {
    curveFromUnsorted(scores, truth).map {
      case (x, y, scores) =>
        var i = 0
        val n = x.length
        val a = Array.ofDim[Double](n)
        while (i < n) {
          val sensitivity = y(i)
          val specificity = 1 - x(i)
          val j = sensitivity + specificity - 1
          a(i) = j
          i += 1
        }
        (x.toVec, y.toVec, scores.toVec, a.toVec)
    }
  }
  def aucFromUnsorted[T](
      scores: Series[T, Double],
      truth: Series[T, Boolean],
      maxFalsePositiveRate: Double = 1d
  ): Either[String, Double] =
    aucFromUnsorted(
      scores.values,
      truth.reindex(scores.index).values,
      maxFalsePositiveRate
    )
  


  def thresholdFromUnsorted[T](
      scores: Series[T, Double],
      truth: Series[T, Boolean]
  ): Either[String, Double] =
    thresholdFromUnsorted(
      scores.values,
      truth.reindex(scores.index).values
    )
  def aucFromUnsorted(
      scores: Vec[Double],
      truth: Vec[Boolean],
      maxFalsePositiveRate: Double
  ): Either[String, Double] = {

    curveFromUnsorted(scores, truth).right.map {
      case (curveX, curveY, _) =>
        val r = auc(curveX, curveY, maxFalsePositiveRate)
        if (r.isNaN) {
          println(Frame(scores, truth.map(v => if (v) 1d else 0d)))
        }
        if (maxFalsePositiveRate == 1d) r
        else {
          // https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3744586/pdf/nihms461170.pdf eq1
          val minArea = 0.5 * math.pow(maxFalsePositiveRate, 2d)
          0.5 * (1d + (r - minArea) / (maxFalsePositiveRate - minArea))
        }
    }
  }
  def thresholdFromUnsorted(
      scores: Vec[Double],
      truth: Vec[Boolean]
  ): Either[String, Double] = {
    import org.saddle.macros.BinOps._
    curveFromUnsorted(scores, truth).right.map {
      case (fpr, tpr, scoresSorted) =>
        val diff = tpr.toVec - fpr.toVec
        scoresSorted.apply(math.min(scoresSorted.length - 1, diff.argmax))
    }
  }
  def thresholdFromUnsorted(
      scores: Vec[Double],
      truth: Vec[Boolean],
      maxFpr: Double
  ): Either[String, Double] = {
    curveFromUnsorted(scores, truth).right.map {
      case (fpr, tpr, scoresSorted) =>
        scoresSorted.apply(fpr.toVec.find(_ < maxFpr).last.getOrElse(0))
    }
  }

  /** Compute ROC points
    *
    * https://www.math.ucdavis.edu/~saito/data/roc/fawcett-roc.pdf
    *  @param scores and truth are sorted by scores descending*/
  def curveFromSorted(
      scores: Array[Double],
      truth: Array[Boolean]
  ) = {
    var fp = 0d
    var tp = 0d
    val x = Array.ofDim[Double](scores.length + 1)
    val y = Array.ofDim[Double](scores.length + 1)
    var scorePrev = Double.NaN
    var i = 0
    val N = scores.length
    val negatives = truth.count(_ == false)
    val positives = N - negatives
    if (negatives == 0 || positives == 0)
      Left("negative or positive truth count is 0")
    else {
      while (i < N) {
        if (scorePrev != scores(i)) {
          x(i) = fp / negatives
          y(i) = tp / positives
          scorePrev = scores(i)
        } else {
          x(i) = x(i - 1)
          y(i) = y(i - 1)
        }
        if (truth(i)) {
          tp += 1
        } else {
          fp += 1
        }
        i += 1
      }
      x(N) = 1d
      y(N) = 1d
      Right((x, y, scores))
    }
  }

  def auc(
      x: Array[Double],
      y: Array[Double],
      maxX: Double
  ) = {
    if (x(0) > maxX) 0d
    else {
      var z = 0
      var s = 0d
      val M = x.length - 1
      var stop = false
      while (z < M && !stop) {
        if (x(z + 1) > maxX) {
          val interpolatedY: Double = {
            val y1 = y(z + 1)
            val y0 = y(z)
            val dy = y1 - y0
            val dx = maxX - x(z)
            val dxtotal = x(z + 1) - x(z)
            y0 + dy * dx / dxtotal
          }
          stop = true
          s += (interpolatedY + y(z)) * 0.5 * (maxX - x(z))
        } else {
          s += (y(z + 1) + y(z)) * 0.5 * (x(z + 1) - x(z))
        }
        z += 1
      }
      s
    }
  }
}
