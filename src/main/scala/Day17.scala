package me.smithson

object Day17 extends Day {

  def minXVectorToReach(pos: Int) = {
    var vector = 1
    var distance = 1
    while (distance < pos) {
      vector += 1
      distance = distance + vector
    }
    vector
  }

  override def Part1(data: Iterator[String]): String = {
    val target = parseData(data)
    val trajectories = trajectoriesToTarget(target)
    trajectories.map(tr => tr._2).max.toString
  }

  override def Part2(data: Iterator[String]): String = {
    val target = parseData(data)
    val trajectories = trajectoriesToTarget(target)
    trajectories.length.toString()
  }

  def trajectoriesToTarget(target: Box) = {
    val minX = minXVectorToReach(target.xRange.min)
    val maxX = target.xRange.max + 1 // will overshoot on first step

    val minY = target.yRange.min
    val maxY = Math.abs(target.yRange.min)
    (minX to maxX).flatMap(x => {
      (minY to maxY).flatMap(y => {
        val tr = Point(x,y)
        val (hitTarget, height) = trajectory(tr, target)
        if (hitTarget) List((tr, height)) else List()
      })
    })
  }

  def trajectory(initialVector: Point, target: Box) = {
    var vector = initialVector
    var position = Point(0, 0)
    var maxHeight = 0
    var hitTarget = false
    while (!hitTarget && (position.x <= target.xRange.max && position.y >= target.yRange.min)) {
      position = Point(position.x + vector.x, position.y + vector.y)
      vector = Point(if (vector.x > 0 ) vector.x - 1 else 0, vector.y -1)
      maxHeight = Math.max(position.y, maxHeight)
      hitTarget = target.contains(position)
    }
    (hitTarget, maxHeight)
  }



  def parseData(data: Iterator[String]) = {
    val line = data.next().substring("target area:".length)
    val ranges = line.split(",").map(r => {
      r.split("=")(1).split("\\.\\.").map(Integer.parseInt).sorted
    })
    val x = ranges(0)
    val y = ranges(1)
    Box((x(0) to x(1)), (y(0) to y(1)))
  }
}

case class Box(xRange: Range, yRange:Range ) {
  def contains(position: Point): Boolean = {
    position.x >= xRange.min && position.x <= xRange.max &&
      position.y >= yRange.min && position.y <= yRange.max
  }

}
