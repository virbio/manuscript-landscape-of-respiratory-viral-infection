package stat.cv

import org.saddle._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport

/** Represents a split of the dataset
  *
  * @param train indices of the training samples
  * @param eval indices of the evaluation samples
  * The two must be disjoint.
  */
case class SplitIdx(train: Vec[Int], eval: Vec[Int]) {
  def intersect(idx: Index[Int]) =
    SplitIdx(
      train = train.filter(idx.contains),
      eval = eval.filter(idx.contains)
    )
}

/* Strategy to split the dataset into cross validation folds */
trait CVSplit { self =>

  def swapTrainAndEval =
    map(split => SplitIdx(train = split.eval, eval = split.train))

  def map(f: SplitIdx => SplitIdx) = new CVSplit {
    def iterator(strata: Vec[Int]*) =
      self
        .iterator(strata: _*)
        .map(f)
  }

  def filter(p: SplitIdx => Boolean) = new CVSplit {
    def iterator(strata: Vec[Int]*) =
      self
        .iterator(strata: _*)
        .filter(p)
  }

  /** Iterator of cross validation folds
    *
    * @param total All indices in the dataset
    * The returned folds must not contain any index not included in `total`
    */
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx]

  /** Feeds the cross validation folds into the supplied training and
    * evaluation functions
    *
    * @param total All indices in the dataset
    * @param train Training function, will be called for each fold with the
    * training indices
    * @param eval Evaluation function, will be called for each fold with the
    * evaluation indices and the result of the training function of the same fold.
    */
  final def loop[Train, Eval](strata: Vec[Int]*)(train: Vec[Int] => Train)(
      eval: (Train, Vec[Int]) => Eval
  ): Iterator[(Train, Eval, SplitIdx)] = iterator(strata: _*).map { split =>
    val trained = train(split.train)
    val evaled = eval(trained, split.eval)
    (trained, evaled, split)
  }

  final def loopPar[Train, Eval](strata: Seq[Vec[Int]], parallelism: Int)(
      train: Vec[Int] => Train
  )(
      eval: (Train, Vec[Int]) => Eval
  ): Vector[(Train, Eval, SplitIdx)] = {
    val pc = iterator(strata: _*).toVector.par
    val forkJoinPool = new java.util.concurrent.ForkJoinPool(parallelism)
    try {
      pc.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
      pc.map { split =>
        val trained = train(split.train)
        val evaled = eval(trained, split.eval)
        (trained, evaled, split)
      }.seq
    } finally {
      forkJoinPool.shutdown()
    }
  }

  /** Similar to loop but does not take eval function */
  final def loop2[Train](strata: Vec[Int]*)(
      train: Vec[Int] => Train
  ): Iterator[(Train, SplitIdx)] = iterator(strata: _*).map { split =>
    val trained = train(split.train)
    (trained, split)
  }
}

case class SplitOnce(ratioOfTrain: Double, rng: scala.util.Random)
    extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = {
    val d = strata.reduce(_ concat _)
    val indices = (0 until d.length).toVector
    val (in, out) =
      rng.shuffle(indices).splitAt((d.length * ratioOfTrain).toInt)
    List(SplitIdx(train = d.take(in.toArray), eval = d.take(out.toArray))).iterator

  }
}
case class SplitOnceBy(
    ratioOfTrain: Double,
    rng: scala.util.Random,
    project: Int => Int
) extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = {
    val d = strata.reduce(_ concat _)
    val p = d.map(project)
    val groups = Series(vec.range(0, d.length), Index(p)).groupBy.groups

    val groupIndices = vec.range(0, groups.length).toSeq
    val (inGroup, outGroup) =
      rng
        .shuffle(groupIndices)
        .splitAt((groupIndices.length * ratioOfTrain).toInt)
    val in = inGroup.toVec.flatMap(i => groups(i)._2.toVec)
    val out = outGroup.toVec.flatMap(i => groups(i)._2.toVec)
    List(SplitIdx(train = d.take(in.toArray), eval = d.take(out.toArray))).iterator

  }
}

case class PremadeSplits(splits: Seq[SplitIdx]) extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = splits.iterator
}

case class FixedSplit(train: Vec[Int], eval: Vec[Int]) extends CVSplit {
  assert((train.toSeq.toSet & eval.toSeq.toSet).isEmpty)
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] =
    List(SplitIdx(train = train, eval = eval)).iterator
}

case class TrainWithIndex(idx: Vec[Int]) extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = {
    val d = strata.reduce(_ concat _)
    List(SplitIdx(train = idx, eval = d.filter(i => !idx.exists(_ == i)))).iterator
  }
}
case class TrainWithoutIndex(idx: Vec[Int]) extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] =
    TrainWithIndex(idx)
      .iterator(strata: _*)
      .map {
        case SplitIdx(eval, train) => SplitIdx(train = train, eval = eval)
      }
}
case object TrainAllTestEmpty extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = {
    val d = strata.reduce(_ concat _)
    List(SplitIdx(train = d, eval = Vec.empty[Int])).iterator
  }
}

case object LeaveOneOut extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] = {
    val d = strata.reduce(_ concat _)
    (0 until d.length).iterator.map { i =>
      SplitIdx(train = d.without(Array(i)), eval = d.take(i))
    }
  }
}

object LeaveKOut {
  def makeSplit(out: Vec[Int], all: Vec[Int]) =
    SplitIdx(train = all.filter(i => !out.exists(_ == i)), eval = out)
}

case class LeaveKOut(ks: Seq[Vec[Int]]) extends CVSplit {
  def iterator(strata: Vec[Int]*): Iterator[SplitIdx] =
    ks.iterator.map(out => LeaveKOut.makeSplit(out, strata.reduce(_ concat _)))
  override def toString = s"LeaveKOut(sizes: ${ks.map(_.length)})"
}

case class KFold(folds: Int, rng: scala.util.Random, replica: Int = 1)
    extends CVSplit {
  private def baseiterator(d: Vec[Int]): Iterator[SplitIdx] = {
    val indices = (0 until d.length).toVector
    (1 to replica iterator) flatMap { r =>
      val shuffled = rng.shuffle(indices)
      val buckets = Vector.fill(folds)(org.saddle.Buffer.empty[Int])
      shuffled.zipWithIndex.foreach {
        case (i, idx) =>
          buckets(idx % folds) += i
      }
      val groups = buckets.map(_.toArray)

      groups.filter(_.nonEmpty).map { i =>
        val out = d.take(i)
        val in = d.without(i)
        SplitIdx(train = in, eval = out)
      }

    }
  }
  def iterator(strata: Vec[Int]*) = {
    val strataIters = strata.map(s => baseiterator(s))
    new Iterator[SplitIdx] {
      def hasNext = strataIters.forall(_.hasNext)
      def next =
        strataIters
          .map(_.next)
          .reduce(
            (x, y) =>
              SplitIdx(
                train = x.train.concat(y.train),
                eval = x.eval.concat(y.eval)
              )
          )
    }
  }
}

trait Selectable[T] {
  def select(idx: Array[Int], t: T): T
}
object Selectable {
  implicit object MatIsSelectable extends Selectable[Mat[Double]] {
    def select(idx: Array[Int], t: Mat[Double]) = t.row(idx)
  }
  implicit def VecIsSelectable[T] = new Selectable[Vec[T]] {
    def select(idx: Array[Int], t: Vec[T]) = t.take(idx)
  }
}

object EvalLoop {

  implicit class SelectableSyntax[T: Selectable](t: T) {
    def select(loc: Array[Int]) = implicitly[Selectable[T]].select(loc, t)
  }
  case class EvalFoldResult[F,T,M](
      metric: T,
      predictionsOfPredictables: List[Mat[Double]],
      outOfSamplePredictions: Mat[Double],
      outOfSampleIdx: Vec[Int],
      predictor: F => Mat[Double],
      model: M
  )
  type TrainingMethod[F,M] =
    (F, Mat[Double]) => (F => Mat[Double],M)

  def cv[T, F: Selectable,M](
      features: F,
      target: Mat[Double],
      strata: Seq[Vec[Int]],
      split: CVSplit,
      predictable: List[F]
  )(
      train: TrainingMethod[F,M],
      eval: (Mat[Double], Mat[Double]) => T
  ): List[EvalFoldResult[F,T,M]] = {
    (split
      .loop(
        strata: _*
      ) { trainIdx =>
        val trainingFeatures = features.select(trainIdx.toArray)
        val trainingTarget = target.row(trainIdx.toArray)

        val trained =
          train(
            trainingFeatures,
            trainingTarget
          )

        trained
      } {
        case ((trained,model), evalIdx) =>
          val evalFeatures = features.select(evalIdx.toArray)
          val evalTarget = target.row(evalIdx.toArray)

          val predictions = trained(evalFeatures)
          val evalMetric = (eval(predictions, evalTarget))

          val predictionOnAllData = predictable.map(trained)

          EvalFoldResult(
            evalMetric,
            predictionOnAllData,
            predictions,
            evalIdx,
            trained,
            model
          )

      })
      .map(_._2)
      .toList
  }

}
