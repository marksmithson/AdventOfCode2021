package me.smithson

class Day4Spec extends DaySpec {
  def day() = { Day4 }
  def part1Result() = {"4512"}
  def part2Result() = {"1924"}

  "Day 4 Parser" should "return numbers and 3 boards" in {
    val parsed = Day4.ParseData(data)
    assert(parsed.numbers.length == 27)
    assert(parsed.boards.length == 3)
  }

  it should "have 5 rows in boards" in {
    val parsed = Day4.ParseData(data)
    parsed.boards.foreach(board => assert(board.rows.length == 5))
  }

  it should "have 5 numbers in boards 1 row 1" in {
    val parsed = Day4.ParseData(data)
    parsed.boards.foreach(board => assert(board.rows(0).length == 5))
  }

  "Day 4 Winner Info" should "be 4512" in {
    val parsed = Day4.ParseData(data)
    val result = parsed.WinnerInfo()
    assert(result == 4512)
  }
}
