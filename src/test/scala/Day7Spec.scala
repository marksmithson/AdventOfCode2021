package me.smithson

class Day7Spec extends DaySpec {
  def day() = { Day7 }
  def part1Result() = {"37"}
  def part2Result() = {"168"}

  "Fuel for position" should "be 37" in {
    val positions = Day7.parseData(data)
    val fuel = Day7.calculateFuelForPosition(positions, 2, Day7.part1FuelCalc)
    assert (fuel == 37)
  }

  "Fuel for position increasing" should "be 206" in {
    val positions = Day7.parseData(data)
    val fuel = Day7.calculateFuelForPosition(positions, 2, Day7.part2FuelCalc)
    assert (fuel == 206)
  }

  "Part 2 fuel to move 11" should "be 66" in {
    assert(Day7.part2FuelCalc(11) == 66)
  }
}
