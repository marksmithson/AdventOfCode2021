package me.smithson

class Day6Spec extends DaySpec {
  def day() = { Day6 }
  def part1Result() = {"5934"}
  def part2Result() = {"26984457539"}

  "Run Generation" should "handle new fish" in {
    val fishAges = Map(0->10L,1->1L,2->2L,3->3L,4->4L,5->5L,6->6L,7->7L,8->8L)
    val result = Day6.runAgeGeneration(fishAges)

    assert(result == Map(0->1L,1->2L,2->3L,3->4L,4->5L,5->6L,6->17L,7->8L,8->10L))
  }
}
