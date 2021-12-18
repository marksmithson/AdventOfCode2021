package me.smithson

class Day8Spec extends DaySpec {
  def day() = { Day8 }
  def part1Result() = {"26"}
  def part2Result() = {"61229"}

  "segment map" should "be correct" in {
    val input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab".split(" ").map(_.toCharArray)

    val map = Day8.segmentMap(input)

    val expected = Map('d' -> 'a', 'e' -> 'b', 'a'-> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')
    assert (map == expected)
  }

  "output using segment map" should "be 5353" in {
    val segmentMap = Map('d' -> 'a', 'e' -> 'b', 'a'-> 'c', 'f' -> 'd', 'g' -> 'e', 'b' -> 'f', 'c' -> 'g')
    val output = "cdfeb fcadb cdfeb cdbaf".split(" ").map(_.toCharArray)

    val result = Day8.actualNumber(segmentMap, output)
    assert (result == 5353)
  }
}
