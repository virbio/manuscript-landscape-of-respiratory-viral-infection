package stat.sparse

import org.saddle._
import org.saddle.order._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import stat.matops._
import scala.collection.mutable
import scala.reflect.ClassTag

object SparseVecOps extends VecOps[SVec] {
  type T = SVec
  def length(t: T): Int = t.length

  def classTag: scala.reflect.ClassTag[stat.sparse.SVec] = implicitly[ClassTag[T]]
  def index(t: stat.sparse.SVec, i: Int): Int = t.values.index.raw(i)
  def nonZeroLength(t: stat.sparse.SVec): Int = t.values.length
  def vv(t: T, t2: Vec[Double]): Double = {
    var s = 0d
    var i = 0
    val v = t.values.toVec.toArray
    val idx = t.values.index.toVec.toArray
    while (i < v.length) {
      val vv = v(i)
      val iv = idx(i)
      s += vv * t2.raw(iv)
      i += 1
    }
    s
  }
  def vv2(t: T, t2: T): Double = {
    var s = 0d
    var i = 0
    val v1 = t.values.toVec
    val v2 = t2.values.toVec
    val idx1 = t.values.index
    val idx2 = t2.values.index
    val n = math.min(v1.length, v2.length)
    while (i < n) {
      val vv1 = v1.raw(i)
      val iv1 = idx1.raw(i)
      if (idx2.contains(iv1)) {
        s += vv1 * v2.raw(idx2.getFirst(iv1))
      }
      i += 1
    }
    s
  }

  def fromElems(s: Array[Double]): SVec =
    SVec(Series(s.toVec, org.saddle.index.IndexIntRange(s.size)), s.size)

  def euclid(t: T, t2: Vec[Double], t2inner: Double): Double =
    stat.kmeans.euclid(t, t2, t2inner)(this)

  def elementWiseMultiplication(t: T, t2: Vec[Double]) = {
    var ar = Array.ofDim[Double](t.values.length)
    var i = 0
    val v = t.values.toVec
    val idx = t.values.index
    var zero = false
    while (i < v.length) {
      val vv = v.raw(i)
      val iv = idx.raw(i)
      val t2v = t2.raw(iv)
      if (t2v == 0d) {
        zero = true
      } else {
        ar(i) = vv * t2v
      }

      i += 1
    }
    val s =
      if (!zero) Series(ar.toVec, idx)
      else Series(ar.toVec, idx).filter(_ != 0d)
    SVec(s, t.length)
  }

  def raw(t: T, i: Int) =
    if (t.values.index.contains(i))
      t.values.toVec.raw(t.values.index.getFirst(i))
    else 0d

  def append(t1: T, t2: T): T = ???
  def toDense(t: T) = stat.sparse.dense(t)

}

object SparseMatOps extends MatOps[SMat] {
  type V = SVec
  type T = SMat
  implicit val vops: VecOps[V] = SparseVecOps

  def squaredEuclideanDistanceMatrix(m: T) : Mat[Double] = {
    val o = outerM(m)
    val norms = Mat(rows(m).map(v => SparseVecOps.vv2(v, v)).toVec)
    import org.saddle.macros.BinOps._
    norms + norms.T - o * 2
  }

  def colMeans(t: stat.sparse.SMat): org.saddle.Vec[Double] = stat.sparse.colmeans(t)

  def fromRows(s: IndexedSeq[SVec]) = s

  def updated(t: T, i: Int, j: Int, v: Double) = {
    val row = t(i)
    val updatedRow = row.copy(
      values =
        if (row.values.contains(j)) row.values.updated(v, j)
        else row.values.concat(Series(j -> v))
    )
    t.updated(i, updatedRow)
  }

  def mv(t: T, v: Vec[Double]): Vec[Double] = {
    val v2 = vec.zeros(t.length).toArray
    val n = v2.length
    var i = 0
    while (i < n) {
      v2(i) = vops.vv(t(i), v)
      i += 1
    }
    Vec(v2)
  }

  def mv(t: T, v: Vec[Double], w: Array[Double]): Vec[Double] = mv(t, v)

  def tmv(t: T, v: Vec[Double]): Vec[Double] = {
    val sums = Array.ofDim[Double](t.head.length)
    var i = 0
    var j = 0
    while (i < t.size) {
      val row = t(i)
      val vec = row.values.toVec
      val idx = row.values.index
      val other = v.raw(i)
      while (j < vec.length) {
        val vv = vec.raw(j)
        val iv = idx.raw(j)
        sums(iv) += vv * other
        j += 1
      }
      j = 0
      i += 1
    }
    sums.toVec
  }

  def innerM(t: T): Mat[Double] = {
    Mat(0 until t.head.length map { colIdx =>
      val r = t.map(r => get(r, colIdx)).toVec

      tmv(t, r)
    }: _*)
  }

  def outerM(t: T): Mat[Double] = {
    val r = t.size
    val ar = Array.ofDim[Double](r * r)
    var i = 0
    var j = 0
    while (i < r) {
      while (j <= i) {
        val v1 = t(i)
        val v2 = t(j)
        val vv = SparseVecOps.vv2(v1, v2)
        ar(i * r + j) = vv
        ar(j * r + i) = vv
        j += 1
      }
      j = 0
      i += 1
    }
    Mat(r, r, ar)
  }
  def outerM2(t: T): SMat = {
    val r = t.size
    val rows_indices = (0 until r).map(_ => Buffer.empty[Int]).toArray
    val rows_values = (0 until r).map(_ => Buffer.empty[Double]).toArray
    var i = 0
    var j = 0
    while (i < r) {
      while (j <= i) {
        val v1 = t(i)
        val v2 = t(j)
        val vv = SparseVecOps.vv2(v1, v2)
        if (vv != 0d) {
          rows_indices(i).+=(j)
          rows_values(i).+=(vv)
          if (i != j) {
            rows_indices(j).+=(i)
            rows_values(j).+=(vv)
          }
        }
        j += 1
      }
      j = 0
      i += 1
    }
    i = 0
    val buffer = mutable.ArrayBuffer[SVec]()
    while (i < r) {
      val id = rows_indices(i)
      val v = rows_values(i)
      buffer += SVec(Series(v.toArray.toVec, Index(id.toArray)), r)
      i += 1
    }
    buffer.toIndexedSeq
  }

  def singularValues(t: T, i: Int): Vec[Double] =
    svd(t, i).sigma

  def mDiagFromLeft(t: T, v: Vec[Double]): T = {
    t.zipWithIndex.map {
      case (row, i) =>
        SVec(row.values * v.raw(i), row.length)
    }
  }
  def mDiagFromRight(t: T, v: Vec[Double]): T = {
    t.map { row =>
      SparseVecOps.elementWiseMultiplication(row, v)
    }
  }

  def tmm(t: T, t2: T): Mat[Double] =
    Mat(0 until t.head.length map { colIdx =>
      val r = t2.map(r => get(r, colIdx)).toVec

      tmv(t, r)
    }: _*)

  def vm(t: SVec, m: Mat[Double], sums: Array[Double], start: Int): Unit = {
    var i = 0
    var j = 0
    val vec = t.values.toVec
    val idx = t.values.index
    while (j < vec.length) {
      val v = vec.raw(j)
      val iv = idx.raw(j)
      while (i < m.numCols) {
        val mat = m.raw(iv, i)
        sums(i + start) += mat * v
        i += 1
      }
      i = 0
      j += 1
    }

  }

  def mm(t: T, m: Mat[Double]): Mat[Double] = {
    val rows = numRows(t)
    val ar = Array.ofDim[Double](m.numCols * rows)
    var i = 0
    var j = 0
    var c = 0
    while (i < t.size) {
      val trow = t(i)
      val prodrow = vm(trow, m, ar, c)
      c += m.numCols
      j = 0
      i += 1
    }
    Mat(rows, m.numCols, ar)
  }

  //       1         2        3
  //       4         5        6
  //
  // a b  a1 + b4 ; a2 + b5; a3+b6
  // c d  c1 + b4 ; c2 + d5
  def mmLeft(left: Mat[Double], right: T): Mat[Double] = {
    val cols = numCols(right)
    val ar = Array.ofDim[Double](cols * left.numRows)
    var i = 0
    var j = 0
    var z = 0
    while (i < right.size) {
      val t = right(i)
      val vec = t.values.toVec
      val idx = t.values.index
      val leftCol = left.col(i)
      while (j < vec.length) {
        val v = vec.raw(j)
        val iv = idx.raw(j)
        val leftColV = leftCol * v
        while (z < leftColV.length) {
          ar(z * cols + j) += leftColV.raw(z)
          z += 1
        }

        z = 0
        j += 1
      }
      j = 0
      i += 1
    }
    Mat(left.numRows, cols, ar)

  }

  def numRows(t: T): Int = stat.sparse.numRows(t)
  def numCols(t: T): Int = stat.sparse.numCols(t)
  def row(t: T, i: Int): SVec = t(i)

  def rowSums(t: T): Vec[Double] = t.map(_.values.toVec.sum2).toVec

  def raw(t: T, i: Int, j: Int): Double = get(t(i), j)
  def rows(t: T): IndexedSeq[V] = t

  def svd(t: T, i: Int): SVDResult = Svd(t, i)(this)

  def mapValues(t: T, f: Double => Double) =
    t.map(row => SVec(row.values.mapValues(f), row.length))

  def only(t: T, rows: Index[Int], cols: Index[Int]) =
    t.map(_.filterAt(cols))
      .zipWithIndex
      .filter(v => rows.contains(v._2))
      .map(_._1)

  // TODO
  def col(t: T, i: Int): SVec =
    SVec(
      Series(
        t.zipWithIndex.view
          .map {
            case (svec, j) =>
              svec.values.first(i).map(x => j -> x)
          }
          .filter(_.isDefined)
          .map(_.get)
          .toSeq: _*
      ),
      t.size
    )

}
