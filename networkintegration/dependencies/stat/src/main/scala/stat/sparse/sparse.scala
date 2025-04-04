package stat
import org.saddle._
import org.saddle.order._
package object sparse {
  type SMat = IndexedSeq[SVec]

  def length(sv: SVec): Int = sv.length
  def idx(sv: SVec): Vec[Int] = index.IndexIntRange(length(sv)).toVec
  def dense(sv: SVec): Vec[Double] = dense(sv, idx(sv))
  def dense(sv: SVec, idx: Vec[Int]): Vec[Double] =
    idx.map(i => sv.values.first(i).getOrElse(0d))

  def get(sv: SVec, idx: Int) =
    if (idx >= sv.length || idx < 0)
      throw new RuntimeException("index out of range")
    else sv.values.first(idx).getOrElse(0d)

  def numCols(sm: SMat): Int =
    if (sm.isEmpty) 0 else sm.map(x => length(x)).max
  def numRows(sm: SMat): Int = sm.length

  private[stat] def colmeans(t: SMat, sums: Array[Double]): Unit = {
    var i = 0
    var j = 0
    val dm = 1d / stat.sparse.numRows(t)
    while (i < t.size) {
      val row = t(i)
      val vec = row.values.toVec
      val idx = row.values.index
      while (j < vec.length) {
        val vv = vec.raw(j)
        val iv = idx.raw(j)
        sums(iv) += vv * dm
        j += 1
      }
      j = 0
      i += 1
    }
  }

  def colmeans(t: SMat): Vec[Double] = {
    val sums = new Array[Double](t.head.length)
    colmeans(t, sums)
    val means: Vec[Double] = sums.toVec
    // SVec(Series(means).filter(_ != 0d), means.length)
    means
  }

  def dense(sm: SMat, rIdx: Vec[Int], cIdx: Vec[Int]): Mat[Double] =
    Mat(rIdx.map(i => sm(i)).map(dense(_, cIdx)).toSeq: _*).T
  def rowIx(sm: SMat): Vec[Int] = index.IndexIntRange(numRows(sm)).toVec
  def colIx(sm: SMat): Vec[Int] = index.IndexIntRange(numCols(sm)).toVec
  def dense(sm: SMat): Mat[Double] = dense(sm, rowIx(sm), colIx(sm))

  def sparseMat(dense: Mat[Double]): SMat = dense.rows.map { denseCol =>
    sparseVec(denseCol)
  }

  def sparseVec(dense: Vec[Double]): SVec = {
    val loc = dense.find(_ != 0d).toArray
    val idx = Index.apply(loc)
    val v = dense.take(loc)
    SVec(Series(v, idx), dense.length)

  }

  def compact(m: SMat) = m.map(_.compact)
  def sorted(m: SMat) = m.map(_.sorted)

}
