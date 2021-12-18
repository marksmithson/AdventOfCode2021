package me.smithson

object Day14 extends Day {
  def Part1(data: Iterator[String]) = {
    val (template, rules) = parseData(data)
    var result = template
    (1 to 10).foreach(_ => {
      result = polymerise(result, rules)
    })
    countDiff(result).toString
  }

  def Part2(data: Iterator[String]) = {
    val (template, r) = parseData(data)
    val rules = compileRules(r)

    val allCounts = template.sliding(2).map(chars => {
      val rule = rules(chars.mkString(""))
      rule.characterCounts(40, rules)
    }).toArray
    val counts = mergeMaps(allCounts)
    val intersectionChars = template.tail.reverse.tail.groupBy(identity).map({ case (key, value) => (key, value.length.toLong) })
    val result = decrementIntersectionChars(intersectionChars, counts)
    diff(result).toString
  }

  def compileRules(rules: Map[String, Char]) = {
    rules.map({ case (key, value) =>
      (key, Rule(key, List(key.substring(0, 1) + value, value + key.substring(1, 2))))
    })
  }

  case class Rule(pair: String, children: List[String]) {
    // include ourselves as item 0 in the list
    private var charCounts: Map[Int, Map[Char, Long]] = Map(0 ->
      pair.toCharArray.groupBy(identity).map({ case (key, value) => (key, value.length.toLong) })
    )

    def characterCounts(depth: Int, rules: Map[String, Rule]): Map[Char, Long] = {
      val existing = charCounts.get(depth)
      existing match {
        case Some(counts) => counts
        case None => {
          val childCounts = children.map(child => {
            val rule = rules(child)
            rule.characterCounts(depth - 1, rules)
          })
          val childCharCounts = mergeMaps(childCounts.toArray)

          // need to remove intersection chars from these counts - we only have 2 so simpler
          val result = decrementIntersectionChars(Map(children.head.toCharArray()(1) -> 1), childCharCounts)
          charCounts = charCounts + (depth -> result)
          result
        }
      }
    }
  }

  def decrementIntersectionChars(intersectionChars:Map[Char, Long], map:Map[Char, Long]) = {
    map.map({
      case (key, value) => {
        if (intersectionChars.contains(key)) (key, value - intersectionChars(key)) else (key, value)
      }
    })
  }

  def countDiff(result: Array[Char]) = {
    val charCounts = result.groupBy(identity).map({ case (key, value) => (key, value.length.toLong) })
    diff(charCounts)
  }

  def diff(result: Map[Char, Long]) = {
    val (_, maxCount) = result.maxBy({ case (_, value) => value })
    val (_, minCount) = result.minBy({ case (_, value) => value })
    maxCount - minCount
  }

  def polymerise(template: Array[Char], rules: Map[String, Char]): Array[Char] = {
    val result = template.sliding(2).flatMap(chars => {
      val insertion = rules.get(chars.mkString(""))
      Array(chars(0), insertion.get)
    }).toArray
    result ++ Array(template.last)
  }

  def parseData(data: Iterator[String]) = {
    val template = data.next().toCharArray

    // skip blank row
    data.next()
    val rules = data.map(row => {
      val parts = row.split("->").map(_.trim)
      (parts(0), parts(1).toCharArray()(0))
    }).toMap
    (template, rules)
  }

  def mergeMaps(maps: Array[Map[Char, Long]]) = {
    maps.reduce((x, y) => {
      val keys: Set[Char] = Set.from(x.keys).union(Set.from(y.keys))
      keys.map(key => {
        val xVal = if (x.keys.exists(_ == key)) x(key) else 0
        val yVal = if (y.keys.exists(_ == key)) y(key) else 0
        (key, xVal + yVal)
      }).toMap
    })
  }
}


