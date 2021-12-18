package me.smithson

object Day4 extends Day {

  def Part1 (data: Iterator[String]) = {
    val bingoData = ParseData(data)
    bingoData.WinnerInfo().toString
  }

  def Part2 (data: Iterator[String]) = {
    val bingoData = ParseData(data)
    bingoData.LastWinnerInfo().toString
  }

  def ParseData(data: Iterator[String]) = {
    // first line is numbers
    val numbers = data.next().split(",").map(Integer.parseInt)

    var boards: List[BingoBoard] = List()
    while (data.hasNext) {
      boards = ParseBoard(data):: boards
    }
    BingoData(numbers, boards.toArray)
  }

  def ParseBoard(data: Iterator[String]) = {
    // skip blank line
    data.next()
    val lines = data.take(5).map(ParseBoardLine).toArray
    BingoBoard(lines)
  }
  def ParseBoardLine(line: String) = {
    (0 to 12 by 3 ).map(ix => {
      Integer.parseInt(line.substring(ix, ix + 2).trim(),10)
    }).toArray
  }
}

case class BingoData (numbers:Array[Int], boards: Array[BingoBoard]){
  def WinnerInfo= () => {
    // go through numbers until we have a winning board
    val numbersIndex = (1 to numbers.length).find(ix => {
      boards.exists(board => board.IsWinner(numbers.slice(0, ix)))
    }).get

    val numbersCalled = numbers.slice(0, numbersIndex)
    val winningBoard = boards.find(board => board.IsWinner(numbersCalled)).get
    val unmarkedNumbers = winningBoard.UnmarkedNumbers(numbersCalled)
    unmarkedNumbers.sum * numbersCalled.last
  }

  def LastWinnerInfo = () => {
    // go backwards through numbers until we have a losing board
    val numbersIndex = (numbers.length to 1 by -1).find(ix => {
      boards.exists(board => !board.IsWinner(numbers.slice(0, ix)))
    }).get

    val numbersCalled = numbers.slice(0, numbersIndex + 1)
    val lastWinningBoard = boards.find(board => !board.IsWinner(numbersCalled.slice(0, numbersCalled.length-1))).get
    val unmarkedNumbers = lastWinningBoard.UnmarkedNumbers(numbersCalled)
    unmarkedNumbers.sum * numbersCalled.last
  }
}

case class BingoBoard (rows: Array[Array[Int]]) {
  def IsWinner = (numbers: Array[Int]) => {
    val cols = rows(0).indices
    rows.exists(row => row.forall(number => numbers.contains(number))) || cols.exists(col => rows.forall(row => numbers.contains(row(col))))
  }

  def UnmarkedNumbers = (numbers: Array[Int]) => {
    rows.flatMap(row => row.filter(num => !numbers.contains(num)))
  }
}
