package me.smithson

object Day11 extends Day {
  def Part1(data: Iterator[String]) = {
    val grid = parseData(data)
    var flashes = 0
    // add one to each cell in grid
    (1 to 100).foreach(ix => {
        step(grid)
        flashes += grid.flatten.count(_ == 0)
      }
    )

    flashes.toString
  }

  def Part2(data: Iterator[String]) = {
    val grid = parseData(data)
    var stepNumber = 0
    while (grid.flatten.count(_ == 0) != 100){
      stepNumber += 1
      step(grid)
    }
    stepNumber.toString
  }

  def step(grid: Array[Array[Int]]) = {
    grid.indices.foreach(rowIx => {
      grid(rowIx).indices.foreach(colIx => grid(rowIx)(colIx) = grid(rowIx)(colIx) + 1)
    })

    while (grid.exists(row => row.contains(10))) {
      grid.indices.foreach(rowIx => {
        val row = grid(rowIx)
        row.indices.foreach(colIx => {
          val cell = row(colIx)
          if (cell == 10) {
            // flash
            row(colIx) = 0
            incrementSurrounding(grid, rowIx, colIx)
          }
        })
      })
    }
  }

  def incrementSurrounding(grid: Array[Array[Int]], rowIx: Int, colIx: Int) = {
    val minRow = if (rowIx > 0) rowIx -1 else rowIx
    val maxRow = if (rowIx < grid.length - 1) rowIx +1 else rowIx
    val minCol = if (colIx > 0) colIx -1 else colIx
    val maxCol = if (colIx < grid(rowIx).length - 1) colIx +1 else colIx

    (minRow to maxRow).foreach(row => {
      (minCol to maxCol).foreach(col => {
        grid(row)(col) = increment(grid(row)(col))
      })
    })
  }

  def increment(i:Int) = i match {
    case 0 => i
    case 10 => i
    case _ => i+1
  }

  def parseData(data: Iterator[String]) = {
    data.map(row => row.toCharArray.map(char => Integer.parseInt(char.toString))).toArray
  }
}
