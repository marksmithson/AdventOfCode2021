package me.smithson

object Day15 extends Day {
  def Part2(data: Iterator[String]) ={
    val grid = parseData(data)

    val largeGrid = (0 to 4).flatMap(rowMultiplier => {
      grid.map(row => {
        val result:Array[Int] = new Array(row.length * 5)
        row.indices.foreach(ix => {
          (0 to 4).foreach(multiplier => {
            result(ix + (row.length * multiplier)) = increment(row(ix), rowMultiplier + multiplier)
          })
        })
        result
      })
    }).toArray
    shortestPath(largeGrid).toString
  }

  def increment(value:Int, times: Int) = value match {
    case value if (value + times > 9) => value + times - 9
    case value => value + times
  }

  def Part1(data: Iterator[String]) = {
    val grid = parseData(data)
    shortestPath(grid).toString
  }

  def shortestPath(grid:Array[Array[Int]]) = {
    // cells with the total risk from end
    val riskGrid = grid.map(row => row.map(_ => Int.MaxValue))

    val end = Point(grid(0).length - 1, grid.length - 1)

    def cellMinRisk(xPos:Int, yPos:Int): Int = {
      val cellRisk = grid(yPos)(xPos)
      if ( xPos == end.x && yPos == end.y)
        cellRisk
      else {
        var result = Int.MaxValue
        if (xPos < end.x){
          result = Math.min(result, riskGrid(yPos)(xPos+1))
        }
        if (yPos < end.y){
          result = Math.min(result, riskGrid(yPos+1)(xPos))
        }
        if (xPos > 0){
          result = Math.min(result, riskGrid(yPos)(xPos-1))
        }
        if (yPos > 0){
          result = Math.min(result, riskGrid(yPos-1)(xPos))
        }
        result + cellRisk
      }
    }

    def updateCellRisk(xPos:Int, yPos:Int) = {
      val cellRisk = cellMinRisk(xPos, yPos)
      val existingRisk = riskGrid(yPos)(xPos)
      if (cellRisk < existingRisk) {
        riskGrid(yPos)(xPos) = cellRisk
        true
      }
      else {
        false
      }
    }

   def processRingCells(xPos:Int, yPos:Int):Boolean = {
      var updated = false
      // do the outer ring cells
      (end.x to xPos by -1).foreach(xCell => {
        updated = updateCellRisk(xCell, yPos) || updated
      })
      (end.y to yPos by -1).foreach(yCell => {
        updated = updateCellRisk(xPos, yCell) || updated
      })
      // now check if the ring inside is changed
      val innerRingXPos = if (xPos < end.x) xPos + 1 else xPos
      val innerRingYPos = if (yPos < end.y) yPos + 1 else yPos
      if (innerRingXPos != xPos || innerRingYPos != yPos) {
        while(processRingCells(innerRingXPos, innerRingYPos)){
          updated = true
        }
      }
     updated
   }

    val maxDelta = Math.max(end.x, end.y)

    // expand out in circles from the end point
    (0 to maxDelta).foreach(delta => {
      val xPos = Math.max(end.x - delta, 0)
      val yPos = Math.max(end.y - delta, 0)
      var updated = processRingCells(xPos, yPos)
      while (updated) {
        // check cells for less risky paths to the left or up, now these have been filled in
        updated = processRingCells(xPos, yPos)
      }
    })

    // return the min risk for the start cell, less the cell risk itself as excluded
    riskGrid(0)(0) - grid(0)(0)
  }

  def parseData(data: Iterator[String]) = {
    data.map(row => row.toCharArray.map(c => Integer.parseInt(c.toString))).toArray
  }
}