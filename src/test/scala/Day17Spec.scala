package me.smithson

import org.scalatest.prop.TableDrivenPropertyChecks

class Day17Spec extends DaySpec with TableDrivenPropertyChecks {
  override def part1Result(): String = "45"

  override def part2Result(): String = "1"

  override def day(): Day = Day17

  "parseData" should "return a box with ranges" in {
    val input = "target area: x=20..30, y=-10..-5"
    val result = Day17.parseData(Iterator(input))

    assert(result.xRange == (20 to 30))
    assert(result.yRange == (-10 to -5))
  }

  val minXChecks = Table(("min", "result"), (1, 1), (2, 2), (3, 2), (4, 3), (6, 3), (7, 4))
  forAll(minXChecks) { (min: Int, result: Int) =>
    "minXToReach" should s"be $result for $min" in {
      assert(Day17.minXVectorToReach(min) == result)
    }
  }
}
