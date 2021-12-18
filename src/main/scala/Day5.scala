package me.smithson

object Day5 extends Day {
  def Part1(data: Iterator[String]) = {
    // have a map of points and increment count for each line
    val lines = ParseData(data)
    // filter horizontal and vertical lines
    val straightLines = FilterStraightLines(lines)
    OverlappingPoints(straightLines).toString
  }

  def Part2(data: Iterator[String]) = {
    val lines = ParseData(data)
    OverlappingPoints(lines).toString
  }

  private def OverlappingPoints(lines:Array[Line]) = {
    val points = lines.flatMap(line => line.Points)
    val numOverlapping = points.groupBy(identity).count { case (_, value) => value.length > 1 }
    numOverlapping
  }

  def ParseData(data: Iterator[String]) = {
    data.map(line => {
      val coords = line.split("->")
      assert(coords.length == 2)
      val startAndEnd = coords.map(coord => {
        val positions = coord.split(",").map(pos => Integer.parseInt(pos.trim()))
        Point(positions(0), positions(1))
      })
      assert(startAndEnd.length == 2)
      Line(startAndEnd(0), startAndEnd(1))
    }).toArray
  }

  def FilterStraightLines(lines: Array[Line]) = {
    lines.filter(_.IsStraight)
  }
}

case class Point(x:Int, y:Int)

case class Line(start:Point, end:Point) {
  def Horizontal = start.y == end.y
  def Vertical = start.x == end.x
  def IsStraight = Horizontal || Vertical
  def Points: IndexedSeq[Point] = {
    if (Horizontal) {
      val step = if (start.x > end.x) -1 else 1
      (start.x to end.x by step).map(x => Point(x, start.y))
    }
    else if (Vertical) {
      val step = if (start.y > end.y) -1 else 1
      (start.y to end.y by step).map(y => Point(start.x, y))
    }
    else {
      // assert we have a 45 degree diagonal
      assert (Math.abs(start.x - end.x) == Math.abs(start.y - end.y))
      val horizontalStep = if (start.x > end.x) -1 else 1
      val verticalStep = if (start.y > end.y) -1 else 1
      (start.x to end.x by horizontalStep).zip(start.y to end.y by verticalStep)
        .map({ case(x,y) => Point(x,y) })
    }
  }
}
