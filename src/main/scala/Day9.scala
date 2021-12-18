package me.smithson

object Day9 extends Day {

  def Part1(data: Iterator[String]) = {
    val grid = parseData(data)
    val colIndices = grid(0).indices

    val lowPoints = grid.indices.flatMap(row =>
      colIndices.map(col => {
        val height = grid(row)(col)
        if (surroundingPoints(grid, Coordinate(row, col)).forall(point => grid(point.row)(point.col) > height)) {
          height
        }
        else -1
      }).filter(_ >= 0)
    )

    val risk = lowPoints.map(_ + 1).sum

    risk.toString
  }

  def Part2(data: Iterator[String]) = {
    val grid = parseData(data)
    basins(grid).map(_.size).sorted.reverse.take(3).product.toString
  }

  def basins(grid: Array[Array[Int]]) = {
    val colIndices = grid(0).indices
    grid.indices.flatMap(row =>
      colIndices.map(col => {
        val height = grid(row)(col)
        val origin = Coordinate(row, col)
        if (surroundingPoints(grid, origin).forall(point => grid(point.row)(point.col) > height)) {
          Basin(height, basinHeights(grid, origin, Set(origin)).size + 1)
        }
        else Basin(-1, 0)
      }).filter(_.size > 0)
    )
  }

  def basinHeights(grid: Array[Array[Int]], origin: Coordinate, seen: Set[Coordinate]): Set[Coordinate] = {
    // include all coordinates until you find a nine
    // in theory it could be possible to have ridges below a 9, but it look like this is not possible in this puzzle
    val points = Set(surroundingPoints(grid, origin)
      .filter(point => grid(point.row)(point.col) < 9 && !seen.contains(point)): _*)

    var result = points
    points.foreach(p => {
      result = result | basinHeights(grid, p, seen | result)
    })
    result
  }

  def parseData(data:Iterator[String]) = {
    data.map(line => line.toCharArray.map(ch => Integer.parseInt(ch.toString))).toArray
  }

  // we only need to consider horizontal and vertical points
  def surroundingPoints(grid: Array[Array[Int]], coord: Coordinate) = {
    var result:List[Coordinate] = List()
    if (coord.row > 0) {
      result = Coordinate(coord.row - 1, coord.col) :: result
    }
    if (coord.col > 0){
      result = Coordinate(coord.row, coord.col - 1) :: result
    }
    if (coord.col < grid(coord.row).length - 1) {
      result = Coordinate(coord.row, coord.col + 1) :: result
    }
    if (coord.row < grid.length - 1) {
      result = Coordinate(coord.row + 1, coord.col) :: result
    }

    result
  }
}

case class Coordinate (row: Int, col:Int)

case class Basin (lowHeight: Int, size: Int)
