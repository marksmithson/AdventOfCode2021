package me.smithson

class Day10Spec extends DaySpec {
  def part1Result() = {"26397"}
  def part2Result() = {"288957"}
  def day() = { Day10 }

  "line check" should "return invalid character" in {
     val line = "{([(<{}[<>[]}>{[]{[(<()>"

    val invalid = Day10.lineCheck(line)

    assert(invalid.get == '}')
  }

  "line check" should "return None for valid line" in {
    val line = "[({(<(())[]>[[{[]{<()<>>"

    val invalid = day().lineCheck(line)

    assert(invalid.isEmpty)
  }

  "score completion" should "be 294" in {
    val chars = Array(']',')','}','>')
    val result = day().scoreCompletion(chars)
    assert (result == 294)
  }
}
