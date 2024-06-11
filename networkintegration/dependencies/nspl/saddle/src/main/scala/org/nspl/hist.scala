package org.nspl.saddle

import org.nspl.data.HistogramData
import org.saddle._

object SaddleHistogramData {

  def makeBoundaries(
      step: Double,
      min: Double,
      max: Double
  ): Seq[(Double, Double)] = {
    assert(max >= min, "max < min")
    assert(step > 0, "step < 0")
    val c = ((max - min) / step).toInt
    if (c == 0) List((min, max))
    else
      0 until c map { i =>
        (min + step * i) -> (min + step * (i + 1))
      } toSeq
  }

  def fromDataAndBoundaries(
      data: Vec[Double],
      binBoundaries: Seq[(Double, Double)]
  ): HistogramData = {

    val min = binBoundaries.minBy(_._1)._1
    val max = binBoundaries.maxBy(_._2)._2
    val data2 = data
      .filter(x => !x.isNaN && !x.isInfinite)
      .filter(z => z <= max && z >= min)

    val displayMax = max

    val bins: Map[(Double, Double, Double), Double] = binBoundaries.map {
      case (x, y) =>
        (x, y, 0.0) -> (if (y == max)
                          data2.countif(z => z <= y && z >= x).toDouble
                        else data2.countif(z => z < y && z >= x).toDouble)
    }.toMap

    val datasize = data2.length

    assert(data2.length.toDouble == bins.map(_._2).sum)

    HistogramData(bins, min, displayMax, bins.values.max, datasize, displayMax)
  }

  def fromDataAndStepMinMax(
      data: Vec[Double],
      step: Double,
      min: Double,
      max: Option[Double]
  ): HistogramData = {
    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    if (data2.isEmpty) {
      HistogramData(
        bins = Map[(Double, Double, Double), Double](),
        minX = 1.0,
        maxX = 1.0,
        maxY = 0.0,
        n = 0,
        lastBinUpperBound = 1.0
      )
    } else {
      val max1 = max.getOrElse {
        data2.max2
      }
      fromDataAndBoundaries(data2, makeBoundaries(step, min, max1))
    }
  }

  def fromDataAndStep(data: Vec[Double], step: Double): HistogramData = {

    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    if (data2.isEmpty) {
      HistogramData(
        bins = Map[(Double, Double, Double), Double](),
        minX = 1.0,
        maxX = 1.0,
        maxY = 0.0,
        n = 0,
        lastBinUpperBound = 1.0
      )
    } else {
      val min = data2.min2
      val max = data2.max2

      fromDataAndStepMinMax(data2, step, min, Some(max))
    }
  }
  def fromDataAndBreaks(data: Vec[Double], breaks: Int): HistogramData = {

    val (step, min, max) = getStep(data, breaks)

    fromDataAndStepMinMax(data, step = step, min = min, max = Some(max))
  }

  def getStep(data: Vec[Double], breaks: Int) = {
    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    if (data2.isEmpty) (1.0, 1d, 1d)
    else {
      val min = data2.min2
      val max = data2.max2
      if (max == min) (1.0, min, min)
      else {
        val breaks2: Int =
          if (breaks <= 0) (math.pow(2d * data.length, 0.5).toInt + 1)
          else breaks
        val ret = (max - min) / breaks2
        assert(
          ret > 0,
          "step < 0 " + ret + " " + breaks2 + " " + data.toString +
            " breaks: " + breaks
        )
        (ret, min, max)
      }
    }
  }
}
