package me.smithson

trait Day {
  def Part1(data: Iterator[String]):String
  def Part2(data: Iterator[String]):String
}

object Main extends App {
  /*val days:List[Day] = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15,
    Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25)*/

  val days:List[Day] = List(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15)
  def runAll() = {
    days.foreach(day => {
      runDay(day)
    })
  }

  def runDay (day: Day) = {
    val className = day.getClass.getSimpleName
    val dayName = className.substring(0, className.length-1)
    println("------------------")
    println(dayName)
    println("------------------")
    println("Part 1:")
    val p1Start = System.currentTimeMillis()
    println(day.Part1(DataLoader.Load(dayName.toLowerCase)))
    println(s"(${System.currentTimeMillis() - p1Start}ms)")
    println("")
    println("Part 2:")
    val p2Start = System.currentTimeMillis()
    println(day.Part2(DataLoader.Load(dayName.toLowerCase)))
    println(s"(${System.currentTimeMillis() - p2Start}ms)")
    println("")
  }

  runDay(Day24)

  println ("COMPLETED")

}