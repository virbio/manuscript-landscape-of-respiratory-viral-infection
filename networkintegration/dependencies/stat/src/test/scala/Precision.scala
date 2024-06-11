package stat.util

import org.saddle._
import org.saddle.linalg._
import org.scalatest.funsuite.{AnyFunSuite => FunSuite}
import stat._
import org.saddle.macros.BinOps._
class PrecisionSuite extends FunSuite {
  test("1") {
    val score = Array(10d, 9d, 8d, 7d, 6d, 5d, 4d, 3d, 2d, 1d)
    val truth =
      Array(true, false, true, false, false, true, false, false, true, true)
    assert(
      Precision.averagePrecisionFromSorted(score, truth) == 0.6222222222222221
    )
  }
  test("2") {
    assert(
      Precision.averagePrecisionFromUnsorted(
        Vec(.9, .8, .7, .6, .55, .54, .53, .52, .51, .505, .4, .39, .38, .37,
          .36, .35, .34, .33, .3, .1),
        Vec(1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0,
          0.0, 0.0, 0.0, 1.0, 0.0, 1.0,
          0.0).map(v => if (v == 1d) true else false)
      ) == 0.7357475805927819
    )
  }
}
