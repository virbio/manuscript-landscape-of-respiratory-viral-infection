package stat.util

import org.saddle._
import org.saddle.linalg._
import org.scalatest.funsuite.{AnyFunSuite => FunSuite}
import stat._
import org.saddle.macros.BinOps._
class ROCSuite extends FunSuite {

  test("threshold") {
    val threshold = ROC
      .thresholdFromUnsorted(
        Vec(.9, .8, .7, .6, .55, .54, .53, .52, .51, .505, .4, .39, .38, .37,
          .36, .35, .34, .33, .3, .1),
        Vec(true, true, false, true, true, true, false, false, true, false,
          true, false, true, false, false, false, true, false, true, false)
      )
      .right
      .get
    assert(threshold == 0.53)
  }
  test("auc") {
    val (x, y, _) = ROC
      .curveFromUnsorted(
        Vec(.9, .8, .7, .6, .55, .54, .53, .52, .51, .505, .4, .39, .38, .37,
          .36, .35, .34, .33, .3, .1),
        Vec(true, true, false, true, true, true, false, false, true, false,
          true, false, true, false, false, false, true, false, true, false)
      )
      .right
      .get
    assert(
      (x zip y).toSeq == Seq(
        (0.0, 0.0),
        (0.0, 0.1),
        (0.0, 0.2),
        (0.1, 0.2),
        (0.1, 0.3),
        (0.1, 0.4),
        (0.1, 0.5),
        (0.2, 0.5),
        (0.3, 0.5),
        (0.3, 0.6),
        (0.4, 0.6),
        (0.4, 0.7),
        (0.5, 0.7),
        (0.5, 0.8),
        (0.6, 0.8),
        (0.7, 0.8),
        (0.8, 0.8),
        (0.8, 0.9),
        (0.9, 0.9),
        (0.9, 1.0),
        (1.0, 1.0)
      )
    )
    assert(ROC.auc(x, y, 1d) == 0.6799999999999999)
  }
  test("auc partial fpr") {
    val (x, y, _) = ROC
      .curveFromUnsorted(
        Vec(.9, .8, .7, .6, .55, .54, .53, .52, .51, .505, .4, .39, .38, .37,
          .36, .35, .34, .33, .3, .1),
        Vec(true, true, false, true, true, true, false, false, true, false,
          true, false, true, false, false, false, true, false, true, false)
      )
      .right
      .get
    assert(
      (x zip y).toSeq == Seq(
        (0.0, 0.0),
        (0.0, 0.1),
        (0.0, 0.2),
        (0.1, 0.2),
        (0.1, 0.3),
        (0.1, 0.4),
        (0.1, 0.5),
        (0.2, 0.5),
        (0.3, 0.5),
        (0.3, 0.6),
        (0.4, 0.6),
        (0.4, 0.7),
        (0.5, 0.7),
        (0.5, 0.8),
        (0.6, 0.8),
        (0.7, 0.8),
        (0.8, 0.8),
        (0.8, 0.9),
        (0.9, 0.9),
        (0.9, 1.0),
        (1.0, 1.0)
      )
    )
    assert(ROC.auc(x, y, 0.1) == 0.020000000000000004)
    assert(ROC.auc(x, y, 0.15) == 0.045)
  }

  
}
