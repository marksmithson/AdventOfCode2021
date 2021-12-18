package me.smithson

object Day7 extends Day {
  def Part1(data: Iterator[String]) = {
    runPart(data, part1FuelCalc).toString
  }
  def Part2(data: Iterator[String]) = {
    runPart(data, part2FuelCalc).toString
  }

  def runPart(data: Iterator[String], fuelCalc: (Int) => Int) = {
    val initialPositions = parseData(data)
    val fuel = (initialPositions.min to initialPositions.max).map(pos => calculateFuelForPosition(initialPositions, pos, fuelCalc))
    fuel.min
  }

  def calculateFuelForPosition(positions:Array[Int], position: Int, fuelCalc: (Int) => Int) = {
    positions.map(pos => fuelCalc(Math.abs(pos - position))).sum
  }

  def part1FuelCalc(distance: Int) = {
    distance
  }

  def part2FuelCalc(distance: Int) = {
    (1 to distance).sum
  }

  def parseData(data: Iterator[String]) = {
    data.map(_.split(",").map(Integer.parseInt)).next()
  }
}
