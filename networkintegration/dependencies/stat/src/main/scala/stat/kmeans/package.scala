package stat

import org.saddle._
import org.saddle.linalg._

import stat.sparse._
import slogging.StrictLogging
import org.saddle.index.IndexDouble
import org.saddle.spire.random.Generator
import stat.matops.MatOps
import stat.matops.VecOps

package object kmeans extends StrictLogging {

 

  def euclid[T: VecOps](t1: T, t2: Vec[Double], t2inner: Double) = {
    var s = t2inner
    var i = 0
    // val a1: Array[Double] = t1.values.toVec.toArray
    val a2 = t2.toArray
    val ops = implicitly[VecOps[T]]
    // val idx1 = t1.values.index.toVec.toArray
    val n1 = ops.nonZeroLength(t1)
    while (i < n1) {
      val iv1 = ops.index(t1, i)
      val vv1 = ops.raw(t1, iv1)
      val vv2 = a2(iv1)
      val d = vv1 - vv2
      s += d * d - vv2 * vv2
      i += 1
    }
    // println(s + " " + ((sparse.dense(t1) - t2) vv (sparse.dense(t1) - t2)))
    val sqrt = math.sqrt(s)
    if (sqrt.isNaN) 0d // can only happen if distance is 0
    else sqrt
  }

  def assign[T: VecOps](v: T, means: Array[(Vec[Double], Double)]): Int = {
    var i = 1
    var m = 0
    var mv = euclid(v, means.head._1, means.head._2)
    while (i < means.size) {
      val mi = means(i)
      val f = euclid(v, mi._1, mi._2)
      if (f < mv) {
        mv = f
        m = i
      }
      i += 1
    }
    m
  }
  def distanceToClosestCenter[T: VecOps](
      v: T,
      means: Array[(Vec[Double], Double)]
  ): Double = {
    var i = 1
    var m = 0
    var mv = euclid(v, means.head._1, means.head._2)
    while (i < means.size) {
      val mi = means(i)
      val f = euclid(v, mi._1, mi._2)
      if (f < mv) {
        mv = f
        m = i
      }
      i += 1
    }
    mv
  }

  def colmeans[T: MatOps](t: T): Vec[Double] =
    implicitly[MatOps[T]].colMeans(t)

  def assignAll[T: MatOps](
      data: T,
      means: Array[Vec[Double]]
  ): Array[Vec[Int]] = {
    val meansWithInner = means.map { i =>
      (i, i vv i)
    }
    val ops: MatOps[T] = implicitly[MatOps[T]]
    val vops: VecOps[ops.V] = ops.vops

    var i = 0
    val N = ops.numRows(data)
    val memberships = 0 until means.size map (
        _ => Buffer.empty[Int](N / means.size)
    )
    while (i < N) {
      val row = ops.row(data, i)
      val membership = assign(row, meansWithInner)(vops)
      memberships(membership).+=(i)
      i += 1
    }
    memberships.map(_.toArray.toVec).toArray
  }

  def update[T: MatOps](
      data: T,
      memberships: Array[Vec[Int]]
  ): Array[Vec[Double]] = {
    val ops = implicitly[MatOps[T]]
    memberships.map { idx =>
      val rows = idx.toSeq.map(i => ops.row(data, i))
      val t: T = ops.fromRows(rows)
      colmeans(t)
    }
  }

  def step[T: MatOps](data: T, means: Array[Vec[Double]]) = {
    val assignment = assignAll(data, means)
    (update(data, assignment), assignment)
  }

  def cost[T: MatOps](
      data: T,
      assignment: Array[Vec[Int]],
      means: Array[Vec[Double]]
  ): Double = {
    val ops = implicitly[MatOps[T]]
    implicit val vops = ops.vops
    ((assignment zip means) map {
      case (assignment, (mean)) =>
        val meand = mean vv mean
        assignment.map { i =>
          val x = euclid(ops.row(data, i), mean, meand)
          x * x
        }.sum
    }).sum
  }

  def squaredDistanceToCenter[T: MatOps](
      data: T,
      centers: Array[Vec[Double]]
  ): Vec[Double] = {
    val meansWithInner = centers.map { i =>
      (i, i vv i)
    }
    implicitly[MatOps[T]]
      .rows(data)
      .map { row =>
        val d = distanceToClosestCenter(row, meansWithInner)(
          implicitly[MatOps[T]].vops.asInstanceOf[VecOps[MatOps[T]#V]]
        )
        d * d
      }
      .toVec

  }

  def apply[T: MatOps](
      data: T,
      init: Array[Vec[Double]],
      it: Int,
      costs: List[Double]
  ): KMeansResult = {

    val (next, assignments) = step(data, init)
    val c = cost(data, assignments, next)
    logger.debug("K-Means cost: {}. It: {}", c, it)

    if (it == 0) {
      val distanceToClosestCenter =
        squaredDistanceToCenter(data, next).map(math.sqrt)

      KMeansResult(
        assignments.zipWithIndex
          .flatMap(x => x._1.toSeq.map(y => y -> x._2))
          .sortBy(_._1)
          .map(_._2)
          .toVec,
        next,
        (c :: costs).reverse.toVec,
        distanceToClosestCenter
      )
    } else apply(data, next, it - 1, c :: costs)
  }

  def random[T: MatOps](
      data: T,
      clusters: Int,
      restarts: Int,
      iterations: Int,
      rng: Generator
  ): KMeansResult = {
    0 until restarts map { _ =>
      val init: Array[Vec[Double]] =
        kmeansPP(data, clusters, rng)._1
      apply(data, init, iterations, Nil)
    } minBy (_.cost)
  }

  def makeCDFFromPDF(pdf: Vec[Double]) = {
    assert(math.abs(pdf.sum2 - 1.0) < 1e-4, pdf.sum2)
    val cdf = pdf.scanLeft(0d)(_ + _)
    cdf
  }

  def sampleFromDiscreteCDF(cdf: Vec[Double], rng: Generator) = {
    val u = rng.nextDouble
    cdf.findOne(c => c > u)
  }

  def kmeansPP[T: MatOps](
      data: T,
      clusters: Int,
      rng: Generator
  ): (Array[Vec[Double]], Array[Int]) = {
    val ops = implicitly[MatOps[T]]
    def pickCenter(centers: List[Vec[Double]]): (Vec[Double], Int) = {
      import org.saddle.ops.BinOps._
      val dsq = squaredDistanceToCenter(data, centers.toArray)
      val dsqSum = dsq.sum
      val pdf = dsq / dsqSum
      val cdf = makeCDFFromPDF(pdf)
      val nextCenter = sampleFromDiscreteCDF(cdf, rng)
      val dense = ops.vops.toDense(ops.row(data, nextCenter))
      (dense, nextCenter)
    }

    val centers = (0 until clusters).foldLeft(List.empty[(Vec[Double], Int)]) {
      (centers, _) =>
        if (centers.isEmpty) {
          val c = rng.nextInt(ops.numRows(data))
          (ops.vops.toDense(ops.row(data, c)), c)
        } :: Nil
        else {
          pickCenter(centers.map(_._1)) :: centers
        }
    }

    (centers.map(_._1).toArray, centers.map(_._2).toArray)

  }

  def apply(data: Mat[Double], init: Mat[Double], it: Int): KMeansResult =
    apply(data, init.rows.toArray, it, Nil)(stat.matops.DenseMatOps)

  def matToSparse(data: Mat[Double]) =
    data.rows.map(x => SVec(Series(x), x.length))

}
