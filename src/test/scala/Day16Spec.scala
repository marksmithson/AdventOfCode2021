package me.smithson

class Day16Spec extends DaySpec {
  def day() = { Day16 }
  def part1Result() = {"31"}
  def part2Result() = {"54"}

  "parseData" should "return string of binary characters" in {
    val input = "D2FE28"
    val result = Day16.parseData(Iterator(input))
    assert (result.sameElements("110100101111111000101000".toCharArray))
  }

  "parsePacket" should "return 2021" in {
    val input = "110100101111111000101000".toCharArray.iterator
    val result = Day16.parsePacket(input)
    println(result)
    //assert(((LiteralPacket)result).value == 2021)
  }

  "versionSum" should "be 16" in {
    val input = "8A004A801A8002F478"
    val result = Day16.Part1(Iterator(input))
    assert(result == "16")
  }
  "versionSum" should "be 12" in {
    val input = "620080001611562C8802118E34"
    val result = Day16.Part1(Iterator(input))
    assert(result == "12")
  }
  "versionSum" should "be 23" in {
    val input = "C0015000016115A2E0802F182340"
    val result = Day16.Part1(Iterator(input))
    assert(result == "23")
  }
  "versionSum" should "be 31" in {
    val input = "A0016C880162017C3686B18A3D4780"
    val result = Day16.Part1(Iterator(input))
    assert(result == "31")
  }

  "calculate" should "be 3" in {
    val input = "C200B40A82"
    val result = Day16.Part2(Iterator(input))
    assert(result == "3")
  }
  "calculate" should "be 54" in {
    val input = "04005AC33890"
    val result = Day16.Part2(Iterator(input))
    assert(result == "54")
  }
  "calculate" should "be 7" in {
    val input = "880086C3E88112"
    val result = Day16.Part2(Iterator(input))
    assert(result == "7")
  }
  "calculate" should "be 9" in {
    val input = "CE00C43D881120"
    val result = Day16.Part2(Iterator(input))
    assert(result == "9")
  }
  "calculate" should "be 1" in {
    val input = "D8005AC2A8F0"
    val result = Day16.Part2(Iterator(input))
    assert(result == "1")
  }
  "calculate" should "be 0 for F600BC2D8F " in {
    val input = "F600BC2D8F"
    val result = Day16.Part2(Iterator(input))
    assert(result == "0")
  }
  "calculate" should "be 0 for 9C005AC2F8F0" in {
    val input = "9C005AC2F8F0"
    val result = Day16.Part2(Iterator(input))
    assert(result == "0")
  }
}
