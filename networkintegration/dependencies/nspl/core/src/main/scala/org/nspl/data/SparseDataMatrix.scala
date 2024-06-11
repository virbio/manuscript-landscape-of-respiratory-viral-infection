package org.nspl.data;

object SparseDataMatrix {
  def fromUnsorted(
      elems: Vector[(Int, Int, Double)],
      numCols: Int,
      numRows: Int,
      missingValue: Double
  ) =
    SparseDataMatrix(
      elems.sortBy(_._2).sortBy(_._1),
      numCols,
      numRows,
      missingValue
    )
}

case class SparseDataMatrix(
    elemsSorted: Vector[(Int, Int, Double)],
    numCols: Int,
    numRows: Int,
    missingValue: Double
) extends DataSource {

  val dimension = 3
  private val n = numCols * numRows

  def iterator =
    ({
      val firstK =
        elemsSorted.headOption.map(tr => tr._1 * numCols + tr._2).getOrElse(0)
      Iterator.fill(firstK)(missingValue)
    } ++ elemsSorted.iterator
      .takeWhile {
        case (i, j, _) =>
          i * numCols + j < n

      }
      .sliding(2)
      .flatMap { group =>
        val (i, j, v) = group(0)
        val (ii, jj, _) = group(1)
        val k = i * numCols + j
        val kk = ii * numCols + jj

        if (kk == k + 1) Iterator.single(v)
        else Iterator.single(v) ++ Iterator.fill(kk - k - 1)(missingValue)
      } ++ elemsSorted
      .takeWhile {
        case (i, j, _) =>
          i * numCols + j < n
      }
      .lastOption
      .orElse(Some((0, 0, missingValue)))
      .iterator
      .map(_._3) ++ {
      val lastK =
        elemsSorted
          .takeWhile {
            case (i, j, _) =>
              i * numCols + j < n
          }
          .lastOption
          .map(tr => tr._1 * numCols + tr._2)
          .getOrElse(0)
      Iterator.fill(n - lastK - 1)(missingValue)
    }).zipWithIndex.map {
      case (value, k) =>
        new Row {
          val K = k;
          val i = K / numCols;
          val j = K % numCols;

          def apply(l: Int) =
            if (l == 0) j.toDouble
            else if (l == 1) i.toDouble
            else value

          def allColumns = Vector(j, i, value)

          def dimension = 3

          def label = ""

          override def toString = value.toString
        }
    }

  def columnMinMax(i: Int) = Some {
    if (i == 0)
      new MinMax {
        def min = 0.0
        def max = numCols - 1d
      } else if (i == 1) new MinMax {
      def min = 0.0
      def max = numRows - 1d
    } else {
      val minV =
        if (elemsSorted.isEmpty) missingValue else elemsSorted.minBy(_._3)._3
      val maxV =
        if (elemsSorted.isEmpty) missingValue else elemsSorted.maxBy(_._3)._3
      new MinMax {
        def min = math.min(minV, missingValue)
        def max = math.max(maxV, missingValue)
      }
    }
  }

}
