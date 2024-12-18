package stat

import org.saddle._

package object matops {
  implicit class PimpMatOps[T](t: T)(implicit val op: MatOps[T]) {
    def mv(v: Vec[Double]): Vec[Double] = op.mv(t, v)
    def mv(v: Vec[Double], w: Array[Double]): Vec[Double] = op.mv(t, v, w)
    def tmv(v: Vec[Double]): Vec[Double] = op.tmv(t, v)
    def innerM: Mat[Double] = op.innerM(t)
    def singularValues(i: Int): Vec[Double] = op.singularValues(t, i)
    def mDiagFromLeft(v: Vec[Double]): T = op.mDiagFromLeft(t, v)
    def mDiagFromRight(v: Vec[Double]): T = op.mDiagFromRight(t, v)
    def tmm(t2: T): Mat[Double] = op.tmm(t, t2)
    def mm(m: Mat[Double]): Mat[Double] = op.mm(t, m)
    def mmLeft(left: Mat[Double]): Mat[Double] = op.mmLeft(left, t)
    def numRows: Int = op.numRows(t)
    def numCols: Int = op.numCols(t)
    def raw(i: Int, j: Int): Double = op.raw(t, i, j)
    def mapValues(f: Double => Double) = op.mapValues(t, f)
    def rowSums = op.rowSums(t)
  }

  implicit class PimpV[T](t: T)(implicit val op: VecOps[T]) {
    def length: Int = op.length(t)
    def vv(t2: Vec[Double]): Double = op.vv(t, t2)
    def vv2(t2: T): Double = op.vv2(t, t2)
    def *(o: Vec[Double]): T = op.elementWiseMultiplication(t, o)
    def raw(i: Int): Double = op.raw(t, i)
    def append(t2: T) = op.append(t, t2)
    def toDense = op.toDense(t)
  }

  implicit def vops[T](implicit m: MatOps[T]): VecOps[m.V] = m.vops
}
