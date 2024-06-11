package stat.util

import org.saddle._
import org.saddle.linalg._
import org.saddle.macros.BinOps._
import stat.sparse

object Distances {
  // instances are rows
  def squaredEuclideanDistance(m1: Mat[Double], m2: Mat[Double]) = {
    val o = m1 mmt m2
    val norms1 = Mat(m1.rows.map(v => v vv v).toVec)
    val norms2 = Mat(m2.rows.map(v => v vv v).toVec)
    norms1 + norms2.T - o * 2
  }
  def squaredEuclideanDistance(m: Mat[Double]) = {
    val o = m.outerM
    val norms = Mat(m.rows.map(v => v vv v).toVec)
    norms + norms.T - o * 2
  }
  def cosineSimilarity(m: Mat[Double]) = {
    val o = m.outerM
    val norms = Mat(m.rows.map(v => v vv v).toVec)
    o / norms / norms.T
  }
  /* entries are 1.0 or 0.0, instances are rows */
  def jaccardSimilarity(m: Mat[Double]) = {
    val a = m.outerM
    val rowSums = Mat(m.rowSums)
    val b = rowSums + rowSums.T - a
    a / b
  }
  /* entries are 1.0 or 0.0, instances are rows */
  def jaccardSimilarity(m1: Mat[Double], m2: Mat[Double]) = {
    val a = m1 mmt m2
    val rowSums1 = Mat(m1.rowSums)
    val rowSums2 = Mat(m2.rowSums)
    val b = rowSums1 + rowSums2.T - a
    a / b
  }
  def jaccardDistance(m: Mat[Double]) = {
    jaccardSimilarity(m).map(v => 1d - v)
  }

  def jaccardSimilaritySparse(m: sparse.SMat) = {
    val a = sparse.SparseMatOps.outerM2(m)
    val rowSums = Mat(sparse.SparseMatOps.rowSums(m))
    val b = (rowSums + rowSums.T)
    var i = 0
    var j = 0
    val n = b.numRows
    while (i < n) {
      while (j < n) {
        val bb = b.raw(i, j)
        val aa = sparse.SparseMatOps.raw(a, i, j)
        val n = aa / (bb - aa)
        b(i, j) = n
        j += 1
      }
      j = 0
      i += 1
    }
    b
  }

}
