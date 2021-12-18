package me.smithson

object Day3 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val diagnosticData = parseData(data)
    val popular = popularBits(diagnosticData)
    var gamma = 0
    var epsilon = 0
    for (ix <- 0 until popular.length) {
      if (popular(ix) == 1) {
        gamma = gamma | bitForIndex(ix)
      }
      else if (popular(ix) == 0) {
        epsilon = epsilon | bitForIndex(ix)
      }
    }
    (gamma * epsilon).toString // 3882564
  }

  override def Part2(data: Iterator[String]): String = {
    val diagnosticData = parseData(data)
    var bitIndex = 0;
    var o2GeneratorData = filterO2GeneratorData(diagnosticData, bitIndex)
    while (o2GeneratorData.length > 1) {
      bitIndex += 1
      o2GeneratorData = filterO2GeneratorData(o2GeneratorData, bitIndex)
    }
    val o2GeneratorRating = bitsToInt(o2GeneratorData.head)

    bitIndex = 0
    var co2ScrubberData = filterCo2ScrubberData(diagnosticData, bitIndex)
    while (co2ScrubberData.length > 1) {
      bitIndex += 1
      co2ScrubberData = filterCo2ScrubberData(co2ScrubberData, bitIndex)
    }
    val co2ScrubberRating = bitsToInt(co2ScrubberData.head)

    (o2GeneratorRating * co2ScrubberRating).toString // 3385170
  }

  def parseData(data: Iterator[String]) = {
    data.map(_.toCharArray.map(_.toString.toInt)).toList
  }

  def bitsToInt(bits: Array[Int]) = {
    Integer.parseInt(bits.mkString(""), 2)
  }

  def filterO2GeneratorData(data: List[Array[Int]], bitIndex: Int) = {
    val popular = popularBits(data)
    var matchBit = popular(bitIndex)
    if (matchBit == -1) matchBit = 1
    data.filter(reading => {
      reading(bitIndex) == matchBit
    })
  }

  def filterCo2ScrubberData(data: List[Array[Int]], bitIndex: Int) = {
    val popular = popularBits(data)
    var matchBit = popular(bitIndex)
    if (matchBit == -1) matchBit = 1
    data.filter(reading => {
      reading(bitIndex) != matchBit
    })
  }

  // returns an array for each bit with the most popular value, -1 if they are the same
  def popularBits(data: List[Array[Int]]) = {
    val sums = Array.fill(data(0).length)(0)
    var count = 0
    data.foreach(reading => {
      count += 1
      for (ix <- reading.indices) sums(ix) += reading(ix)
    })

    sums.map(sum => {
      if (sum > count / 2.0)
        1
      else if (sum < count / 2.0)
        0
      else
        -1
    })
  }

  def bitForIndex(ix: Int) = {
    Math.max(math.pow(2, (11 - ix)), 1).toInt  // 2 pow 0 = 0, should be 1 for least significant bit
  }
}
