package org.nspl

object LabelLayout {
  def intersection(p1: Point, p2: Point, p3: Point, p4: Point) = {
    val x1 = p1.x
    val y1 = p1.y
    val x2 = p2.x
    val y2 = p2.y
    val x3 = p3.x
    val y3 = p3.y
    val x4 = p4.x
    val y4 = p4.y

    val t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
    val u = -1 * ((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))

    if (t >= 0 && t <= 1 && u >= 0 && u <= 1) {
      val px1 = x1 + t * (x2 - x1)
      val py1 = y1 + t * (y2 - y1)

      val px2 = x3 + u * (x4 - x3)
      val py2 = y3 + u * (y4 - y3)
      Some(Point(px1, py1))
    } else None
  }

  def distance(ax: Double, ay: Double, px: Double, py: Double): Double = {
    val a = math.abs(ax - px)
    val b = math.abs(ay - py)
    math.sqrt(a * a + b * b)
  }

  def distance(r1: Bounds, r2: Bounds): Double = {
    val x1 = r1.x
    val y1 = r1.y
    val x1b = r1.maxX
    val y1b = r1.maxY
    val x2 = r2.x
    val y2 = r2.y
    val x2b = r2.maxX
    val y2b = r2.maxY

    val left = x2b < x1
    val right = x1b < x2
    val bottom = y2b < y1
    val top = y1b < y2
    if (top && left)
      distance(x1, y1b, x2b, y2)
    else if (left && bottom)
      distance(x1, y1, x2b, y2b)
    else if (bottom && right)
      distance(x1b, y1, x2, y2b)
    else if (right && top)
      distance(x1b, y1b, x2, y2)
    else if (left)
      x1 - x2b
    else if (right)
      x2 - x1b
    else if (bottom) y1 - y2b
    else if (top) y2 - y1b
    else 0d
  }

  def line(r1: Bounds, r2: Bounds) = {
    val x1 = r1.x
    val y1 = r1.y
    val x1b = r1.maxX
    val y1b = r1.maxY
    val x2 = r2.x
    val y2 = r2.y
    val x2b = r2.maxX
    val y2b = r2.maxY

    val center1 = Point(r1.centerX, r1.centerY)
    val center2 = Point(r2.centerX, r2.centerY)

    val top1 = (Point(x1, y1b), Point(x1b, y1b))
    val bottom1 = (Point(x1, y1), Point(x1b, y1))
    val left1 = (Point(x1, y1), Point(x1, y1b))
    val right1 = (Point(x1b, y1), Point(x1b, y1b))

    val top2 = (Point(x2, y2b), Point(x2b, y2b))
    val bottom2 = (Point(x2, y2), Point(x2b, y2))
    val left2 = (Point(x2, y2), Point(x2, y2b))
    val right2 = (Point(x2b, y2), Point(x2b, y2b))

    val p1 = List(top1, bottom1, left1, right1)
      .map(l => intersection(center1, center2, l._1, l._2))
      .find(_.isDefined)
      .flatten

    val p2 = List(top2, bottom2, left2, right2)
      .map(l => intersection(center1, center2, l._1, l._2))
      .find(_.isDefined)
      .flatten

    p1.flatMap(p1 => p2.map(p2 => (p1, p2)))

  }

  def many(points: Seq[(Bounds, Bounds)], maxSteps: Int) = {
    var state = points.toArray
    var i = 1
    val n = maxSteps
    while (i < n) {
      var j = 0
      while (j < state.length) {
        val (point, rec) = state(j)
        state(j) = one(point, rec, state, j)

        j += 1
      }
      i += 1
    }

    state.map {
      case (point, rec) =>
        (rec, LabelLayout.line(point, rec))
    }
  }
  def one(
      point: Bounds,
      rectangle: Bounds,
      others: Array[(Bounds, Bounds)],
      self: Int
  ) = {

    val rcenterX = rectangle.centerX
    val rcenterY = rectangle.centerY

    def calculateForces = {
      var x = 0d
      var y = 0d
      val lr = 0.01

      def updateForce(b: Bounds) = {
        val dist = distance(b, rectangle)
        if (dist < 200 && b != rectangle) {
          val repel = math.min(100d, 100d / (dist * dist))
          val bcenterX = b.centerX
          val bcenterY = b.centerY

          val centerDist = distance(bcenterX, bcenterY, rcenterX, rcenterY)
          val dirX = (bcenterX - rcenterX) / centerDist
          val dirY = (bcenterY - rcenterY) / centerDist
          x -= repel * dirX * lr
          y -= repel * dirY * lr
        }
      }

      var i = 0
      while (i < others.length) {
        if (i != self) {
          val (otherPoint, otherRectangle) = others(i)
          if (math.abs(otherPoint.x - point.x) < 50 && math.abs(
                otherPoint.y - point.y
              ) < 50) {
            updateForce(otherPoint)
            updateForce(otherRectangle)
          }
        }
        i += 1
      }

      val attraction = distance(rectangle, point) * 10
      val pcenterX = point.centerX
      val pcenterY = point.centerY
      val centerDist =
        distance(pcenterX, pcenterY, rcenterX, rcenterY)
      val dirX = (pcenterX - rcenterX) / centerDist
      val dirY = (pcenterY - rcenterY) / centerDist
      x += attraction * dirX * lr
      y += attraction * dirY * lr
      (x, y)
    }
    val f = calculateForces
    (point, rectangle.copy(x = rectangle.x + f._1, y = rectangle.y + f._2))

  }

}
