package me.smithson

import scala.collection.mutable.Stack

object Day10 extends Day {
  def Part1(data: Iterator[String]) = {
    val invalid = data.map(lineCheck).filter(_.isDefined).map(_.get)
    invalid.map(invalidCharPoints(_)).sum.toString
  }

  def Part2(data: Iterator[String]) = {
    val validLines = data.filter(line => lineCheck(line).isEmpty)
    val lineCompletions = validLines.map(completionCharacters)
    val scores = lineCompletions.map(scoreCompletion).toList.sorted
    scores(scores.length / 2).toString
  }

  val invalidCharPoints = Map('}'->1197, ']'->57, '>'->25137,')'->3)
  val completionPoints = Map(')'->1, ']'->2, '}'->3,'>'->4)

  def scoreCompletion (chars: Array[Char]) = {
    var score = 0L
    chars.foreach(char => {
      score = (score * 5L) + completionPoints(char)
    })
    score
  }
  def completionCharacters(line:String): Array[Char] = {
    val stack:Stack[Char] = Stack()
    line.toCharArray.foreach(char => {
      if (isOpening(char)) {
        stack.push(char)
      }
      else {
        // we know it is valid
        stack.pop()
      }
    })
    stack.toArray.map(braceChars(_))
  }

  def lineCheck(line:String) : Option[Char] = {
    val stack:Stack[Char] = Stack()

    line.toCharArray.find(char => {
      if (isOpening(char)){
        stack.push(char)
        false
      }
      else if (stack.isEmpty) {
        true
      } else {
        braceChars(stack.pop()) != char
      }
    })
  }

  val braceChars = Map('{'->'}', '['->']', '<'->'>','('->')')
  def isOpening(char:Char) = {
    braceChars.keys.exists(_ == char)
  }
}
