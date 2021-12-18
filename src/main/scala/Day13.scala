package me.smithson

object Day13 extends Day {

  def Part1(data: Iterator[String]) = {
    val (grid, folds) = parseData(data)

    val fold = folds(0)
    val foldedGrid = doFold(grid, fold)
    foldedGrid.size.toString
  }

  def Part2(data: Iterator[String]) = {
    val (grid, folds) = parseData(data)

    var result = grid
    folds.foreach(fold => {
      result = doFold(result, fold)
    })
    printGrid(result)
  }

  def printGrid(grid: List[Array[Int]]) = {
    val builder = new StringBuilder()
    val bounds = grid.reduce((result:Array[Int], point:Array[Int])=> {
      Array(Math.max(result(0), point(0)), Math.max(result(1), point(1)))
    })
    // rows
    (0 to bounds(1)).foreach(row => {
      (0 to bounds(0)).foreach(col => {
        val cell = grid.find(p => p(0) == col && p(1) == row)
        cell match {
          case None => builder.append(" ")
          case Some(_) => builder.append('#')
        }
      })
      builder.append("\n")
    })
    builder.toString()
  }

  def doFold(grid: List[Array[Int]], fold: (String, Int)) = {
    // calculate subgrid which is to be folded
    val foldCoord = if (fold._1 == "x") 0 else 1
    val nonFoldCoord = if (fold._1 == "x") 1 else 0
    val mirrorAmt = (fold._2 * 2)
    val resultGrid = grid.filter(point => point(foldCoord) < fold._2)
    val foldGrid = grid.filter(point => {
      point(foldCoord) > fold._2
    }).map(point => { // transform the grid
      val result: Array[Int] = Array(0, 0)
      result(foldCoord) = 0 - (point(foldCoord) - mirrorAmt)
      result(nonFoldCoord) = point(nonFoldCoord)
      result
    })

    val newPoints = foldGrid.filter(point => !resultGrid.exists(p => p.sameElements(point)))
    resultGrid ++ newPoints
  }

  def parseData(data: Iterator[String]) = {
    val list = data.toList // so we can do multiple passes
    val grid = list.filter(row => row.contains(",")).map(_.split(",").map(pos => Integer.parseInt(pos)))
    val folds = list.filter(row => row.startsWith("fold along")).map(row => {
      val foldInfo = row.substring(11).split("=")
      (foldInfo(0), Integer.parseInt(foldInfo(1)))
    })
    (grid, folds)
  }
}
