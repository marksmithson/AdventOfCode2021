package me.smithson

class Day5Spec  extends DaySpec {
  def day() = { Day5 }
  def part1Result() = {"5"}
  def part2Result() = {"12"}
  "Day 5 Parser" should "return 10 lines" in {
    val result = Day5.ParseData(data)

    assert(result.length == 10)
  }

  it should "return lines with start and end" in {
    val result = Day5.ParseData(data)

    assert(result(0).start == Point(0,9))
    assert(result(0).end == Point(5,9))
  }

  "FilterStraightLines" should "remove diagonal lines" in {
    val lines = Array(
      Line(Point(0,0), Point(0,10)),
      Line(Point(0,0), Point(10,0)),
      Line(Point(0,0), Point(10,10)),
    )
    val result = Day5.FilterStraightLines(lines)
    assert(result.length == 2)
  }
  "Line.Points" should "return points on Horizontal line" in {
    val result = Line(Point(0,0), Point(2,0)).Points

    assert(result.length == 3)
    assert(result(0) == Point(0,0))
    assert(result(1) == Point(1,0))
    assert(result(2) == Point(2,0))
  }
  "Line.Points" should "return points on Vertical line" in {
    val result = Line(Point(0,0), Point(0, 2)).Points

    assert(result.length == 3)
    assert(result(0) == Point(0,0))
    assert(result(1) == Point(0,1))
    assert(result(2) == Point(0,2))
  }

  "Line.Points" should "return points on a 45 degree diagonal line" in {
    val result = Line(Point(0,0), Point(2, 2)).Points

    assert(result.length == 3)
    assert(result(0) == Point(0,0))
    assert(result(1) == Point(1,1))
    assert(result(2) == Point(2,2))
  }
}
