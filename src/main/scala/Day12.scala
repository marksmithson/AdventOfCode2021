package me.smithson

object Day12 extends Day {
  def Part1(data: Iterator[String]) = {
    val connections = parseData(data).toList
    val paths = pathsToEnd(connections, List("start"), "", "start")
    paths.size.toString
  }

  def Part2(data: Iterator[String]) = {
    val connections = parseData(data).toList

    val smallCaves = Set.from(connections.flatten.filter(cave => isLowercase(cave) && cave != "start" && cave != "end"))
    var allPaths:Set[List[String]] = Set()
    val sizes = smallCaves.toList.map(cave => {
      val newPaths = Set(pathsToEnd(connections, List("start"), cave, "start"): _*)
      allPaths = allPaths ++ newPaths
    })

    allPaths.size.toString
  }

  def pathsToEnd(connections: List[Array[String]], visited: List[String], doubleVisitCave: String, start:String): List[List[String]] = {
    if (start == "end") {
      List(visited)
    }
    else {
      val outgoing = connections
        .filter(con => con(0) == start || con(1) == start)
        .filter(con => {
          val next = if (con(0) == start) con(1) else con(0)
          canVisit(next, visited, doubleVisitCave)
        })
      val result = outgoing.flatMap(con => {
        val next = if (con(0) == start) con(1) else con(0)
        pathsToEnd(connections, next :: visited, doubleVisitCave, next)
      })
      result
    }
  }

  def canVisit(cave:String, visited:List[String], doubleVisitCave: String) = {
    if (isLowercase(cave)){
      val visits = visited.count(_ == cave)
      if (doubleVisitCave == cave) visits < 2 else visits < 1
    }
    else {
      true
    }
  }

  def isLowercase(value: String) = {
    value.toLowerCase() == value
  }

  def parseData(data: Iterator[String]) = {
    data.map(row => row.split("-"))
  }
}
