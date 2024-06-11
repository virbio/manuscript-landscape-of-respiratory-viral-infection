package stat.sparse

import org.saddle._
import org.saddle.order._
import scala.collection.mutable

case class SVec(values: Series[Int, Double], length: Int) {
  def compact = copy(values = values.filter(_ != 0d))
  def sorted = copy(values = values.sortedIx)
  def filterAt(at: Index[Int]) = copy(values.filterIx(i => at.contains(i)))
}

class SparseMatrixBuilder(numRows: Int, numCols: Int) {
  private val rows_indices =
    (0 until numRows).map(_ => Buffer.empty[Int]).toArray
  private val rows_values =
    (0 until numRows).map(_ => Buffer.empty[Double]).toArray

  def push(row: Int, col: Int, v: Double) = {
    if (col >= numCols) throw new RuntimeException("index out of bounds")
    rows_indices(row).+=(col)
    rows_values(row).+=(v)
  }

  def result: SMat = {
    var i = 0
    val buffer = mutable.ArrayBuffer[SVec]()
    while (i < numRows) {
      val id = rows_indices(i)
      val v = rows_values(i)
      val idx = Index(id.toArray)
      assert(
        idx.isUnique,
        "Row indices in sparse matrix builder are not unique. The same (row,col) was pushed twice into the builder."
      )
      buffer += SVec(
        Series(v.toArray.toVec, idx),
        numCols
      )
      i += 1
    }
    buffer.toIndexedSeq
  }
}
