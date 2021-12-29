package me.smithson

object Positions extends Enumeration {
  type Position = Value

  val Empty = Value(".")
  val Down = Value("v")
  val Right = Value(">")
}

object Day25 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val originalMap = parseData(data)

    var (map, moves) = step(originalMap)
    var steps = 1
    while (moves > 0) {
      steps += 1
      val(newMap, stepMoves) = step(map)
      map = newMap
      moves = stepMoves
    }
    steps.toString
  }

  def step(map:Array[Array[Positions.Value]]) = {
    val(eastMap, eMoves) = moveEast(map)
    val(newMap, sMoves) = moveSouth(eastMap)
    (newMap, eMoves + sMoves)
  }

  def moveEast(map:Array[Array[Positions.Value]]) = {
    var moves = 0
    val newMap = map.map(row => {
      row.indices.map(ix => {
        // what should we have at this index
        row(ix) match {
          case Positions.Down => Positions.Down
          case Positions.Empty => {
            val prevIx = if (ix == 0) row.length - 1 else ix - 1
            if (row(prevIx) == Positions.Right){
              moves += 1
              Positions.Right
            }
            else {
              Positions.Empty
            }
          }
          case Positions.Right => {
            val nextIx = if (ix == row.length - 1) 0 else ix + 1
            if (row(nextIx) == Positions.Empty){
              Positions.Empty
            }
            else {
              Positions.Right
            }
          }
        }
      }).toArray
    })
    (newMap, moves)
  }

  def moveSouth(map:Array[Array[Positions.Value]]) = {
    var moves = 0
    val newMap = map.zipWithIndex.map({ case (row, rowIx) => {
      row.indices.map(ix => {
        // what should we have at this index
        row(ix) match {
          case Positions.Down => {
            val nextRowIx = if(rowIx == map.length - 1) 0 else rowIx + 1
            if (map(nextRowIx)(ix) == Positions.Empty){
              Positions.Empty
            }
            else {
              Positions.Down
            }
          }
          case Positions.Empty => {
            val prevRowIx = if(rowIx == 0) map.length - 1 else rowIx - 1
            if (map(prevRowIx)(ix) == Positions.Down) {
              moves += 1
              Positions.Down
            }
            else {
              Positions.Empty
            }
          }
          case Positions.Right => Positions.Right
        }
      }).toArray
    }
    })
    (newMap, moves)
  }

  override def Part2(data: Iterator[String]): String = ""

  def parseData(data: Iterator[String]) = {
    data.map(row => {
      row.toCharArray.map {
        case '.' => Positions.Empty
        case '>' => Positions.Right
        case 'v' => Positions.Down
      }
    }).toArray
  }
}




