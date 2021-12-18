package me.smithson

object Day1 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val readings = parseData(data)
    calcIncreases(readings).toString
  }

  override def Part2(data: Iterator[String]): String = {
    val readings = parseData(data)
    val windows = readings.sliding(3).map(_.sum)
    calcIncreases(windows).toString
  }

  def parseData(data: Iterator[String]) = {
    data.map(Integer.parseInt)
  }

  def calcIncreases(values: Iterator[Int]) = {
    // use a 2 value sliding window so we can compare to the previous value with an iterator
    values.sliding(2).map(
      window => if (window(1) > window.head) 1 else 0
    ).sum
  }
}
