package me.smithson

object Day8 extends Day {
  def Part1(data: Iterator[String]) = {
    val digits = parseData(data)
    val matchingLengths = List(2,3,4,7)
    val matchingPatterns = digits.flatMap(digit => digit.outputs.filter(pattern => matchingLengths.contains(pattern.length)))
    matchingPatterns.length.toString
  }

  def Part2(data: Iterator[String]) = {
    val digits = parseData(data)
    val result = digits.map(digit => {
      actualNumber(segmentMap(digit.inputs), digit.outputs)
    })
    result.sum.toString
  }

  def parseData (data: Iterator[String]) = {
    data.map(line => {
      val lineDigits = line.split("\\|").map(_.split(" ").map(_.trim.toCharArray).filter(_.length > 0))
      assert(lineDigits.length == 2)
      Digits(lineDigits(0), lineDigits(1))
    })
  }

  def segmentMap (inputs:Array[Array[Char]]) = {
    // digits 1,4,7,8 are known from length
    val one = inputs.find(_.length == 2).get
    val four = inputs.find(_.length == 4).get
    val seven = inputs.find(_.length == 3).get
    val eight = inputs.find(_.length == 7).get
    // digit 6 is a 6 digit pattern including one of the digits from 1
    val six = inputs.find( segments => {
      segments.length == 6 && one.count(segments.contains(_)) == 1
    } ).get
    // digit 3 is a 5 digit pattern containing all the digits from 7
    val three = inputs.find(segments => {
      segments.length == 5 && seven.forall(seg => segments.contains(seg))
    }).get
    var result = Map[Char, Char](
      seven.find(!one.contains(_)).get -> 'a', // segment a is the segment from 7 not in 1
      four.find(!three.contains(_)).get -> 'b', // segment b is the segment from 4 not in 3
      one.find(!six.contains(_)).get -> 'c', // segment c is the segment from 1 not in 6
      one.find(six.contains(_)).get -> 'f' // segment f is the segment from 1 which is in 6
    )
    def missingSegment(digit: Array[Char], segments: Iterable[Char]) = {
      digit.find(seg => !segments.exists(_ == seg)).get
    }
    result = result + (missingSegment(four, result.keys) -> 'd') // segment d is the missing segment from 4
    result = result + (missingSegment(three, result.keys) -> 'g') // segment g is the missing segment from 3
    result = result + (missingSegment(eight, result.keys) -> 'e') // segment e is the missing segment from 8
    result
  }

  val segmentsToDecimal = Map(
    "abcefg" -> '0',
    "cf" -> '1',
    "acdeg" -> '2',
    "acdfg" -> '3',
    "bcdf" -> '4',
    "abdfg" -> '5',
    "abdefg" -> '6',
    "acf" -> '7',
    "abcdefg" -> '8',
    "abcdfg" -> '9',
  )

  def actualNumber(segmentMap: Map[Char, Char], output: Array[Array[Char]]) = {
    assert(output.length == 4)
    Integer.parseInt(
      output.map(digit => digit.map(segmentMap(_)).sorted)
        .map(seg => segmentsToDecimal(seg.mkString(""))).mkString("")
    )
  }
}

case class Digits (inputs: Array[Array[Char]], outputs: Array[Array[Char]])
