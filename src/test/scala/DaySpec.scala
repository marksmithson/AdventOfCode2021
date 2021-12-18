package me.smithson

import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec

abstract class DaySpec extends AnyFlatSpec with BeforeAndAfterEach {
  var data: Iterator[String] = null
  def part1Result(): String
  def part2Result(): String
  def day(): Day

  override def beforeEach() {
    val className = this.getClass.getSimpleName
    val dayName = className.substring(0, className.indexOf("Spec")).toLowerCase
    data = DataLoader.Load(s"${dayName}-test")
  }

  "Part 1 result" should s"be ${part1Result()}" in {
    val result = day().Part1(data)
    assert(result == part1Result())
  }
  "Part 2 result" should s"be ${part2Result()}" in {
    val result = day().Part2(data)
    assert(result == part2Result())
  }
}
