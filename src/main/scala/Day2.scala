package me.smithson

object Day2 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val navigation = parseData(data)
    var position = 0
    var depth = 0

    navigation.foreach(nav => {
      nav.operation match {
        case "forward" => position += nav.magnitude
        case "down" => depth += nav.magnitude
        case "up" => depth -= nav.magnitude
        case other => println(s"Invalid operation ${other}")
      }
    })
    (position * depth).toString
  }

  override def Part2(data: Iterator[String]): String = {
    val navigation = parseData(data)
    var position = 0
    var depth = 0
    var aim = 0

    navigation.foreach(nav => {
      nav.operation match {
        case "forward" => {
          position += nav.magnitude
          depth += aim * nav.magnitude
        }
        case "down" => aim += nav.magnitude
        case "up" => aim -= nav.magnitude
        case other => println(s"Invalid operation ${other}")
      }
    })
    (position * depth).toString
  }

  def parseData(data: Iterator[String]) = {
    data.map(parseNavigationCommand)
  }

  def parseNavigationCommand(commandLine: String) = {
    val commandParts = commandLine.split(' ')
    new NavigationCommand(commandParts(0), Integer.parseInt(commandParts(1)))
  }
}

class NavigationCommand (val operation:String, val magnitude:Int)
