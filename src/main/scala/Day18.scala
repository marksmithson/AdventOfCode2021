package me.smithson

object Day18 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val numbers = parseData(data)
    val result = numbers.reduce((a,b) => {
      //println(s"= $a")
      //println(s"+ $b")
      add(a, b)
    })
    //println(result)
    result.magnitude.toString
  }

  override def Part2(data: Iterator[String]): String = {
    val numbers = parseData(data).toList
    val combinations = numbers.flatMap(x => { numbers.map(y => (x,y))})
    val result = combinations.map((x) => {
      Day18.add(x._1, x._2).magnitude
    })
    result.max.toString
  }

  def parseData (data: Iterator[String]) = {
    data.map(line => parseNumber(line.toCharArray.iterator))
  }

  def clone(n:SnailFishNumber): SnailFishNumber = {
    parseNumber(n.toString.iterator)
  }

  def add(a:SnailFishNumber, b:SnailFishNumber): SnailFishNumber = {
    val r = Pair(clone(a),clone(b))
    reduce(r)
  }

  def reduce (n:Pair) = {
    var changed = true
    //println(n)
    while (changed) {
      changed = explode(n)
      if (!changed) {
        changed = split(n)
      }
      //println(n)
    }
    n
  }

  def split(n:Pair): Boolean = {
    def toSplit = regularToSplit(n)
    if (toSplit.isDefined){
      //println (s"split $toSplit")
      val s = toSplit.get
      val x = Math.floor(s.value.toDouble / 2).toInt
      val y = Math.ceil(s.value.toDouble / 2).toInt
      val r = Pair(Regular(x), Regular(y))

      replaceNumber(s, r)

      true
    }
    else {
      false
    }
  }

  def replaceNumber(n:SnailFishNumber, r: SnailFishNumber) = {
    val p = n.parent.get
    r.parent = Some(p)
    p match {
      case px if px.x == n => p.x = r
      case py if py.y == n => p.y = r
    }
  }

  def regularToSplit(n:Pair): Option[Regular] = {
    var result = n.x match {
      case r: Regular if r.value > 9 => Some(r)
      case p: Pair => regularToSplit(p)
      case _: Regular => None
    }
    if (result.isEmpty) {
      result = n.y match {
        case r: Regular if r.value > 9 => Some(r)
        case p: Pair => regularToSplit(p)
        case _: Regular => None
      }
    }
    result
  }

  def explode(n: Pair): Boolean = {
    def toExplode = pairToExplode(n, 1)
    if (toExplode.isDefined){
      val ex = toExplode.get
      //println (s"explode $ex")
      val left = leftNumberParent(ex)
      val right = rightNumberParent(ex)

      if (left.isDefined) {
        left.get.value += ex.x.asInstanceOf[Regular].value
      }
      if (right.isDefined) {
        right.get.value += ex.y.asInstanceOf[Regular].value
      }

      replaceNumber(ex, Regular(0))

      true
    } else {
      false
    }
  }

  def leftNumberParent(n:Pair): Option[Regular] = {
    if (n.parent.isEmpty) None
    else {
      val parent = n.parent.get
      parent.x match {
        case p if p eq n => leftNumberParent(parent)
        case p: Pair => rightNumberChild(p)
        case p: Regular => Some(p)
      }
    }
  }

  def rightNumberChild(n:Pair): Option[Regular] = {
    n.y match {
      case p: Regular => Some(p)
      case p: Pair => rightNumberChild(p)
    }
  }

  def rightNumberParent(n:Pair): Option[Regular] = {
    if (n.parent.isEmpty) None
    else {
      val parent = n.parent.get
      parent.y match {
        case p if p eq n => rightNumberParent(parent)
        case p: Pair => leftNumberChild(p)
        case p: Regular => Some(p)
      }
    }
  }

  def leftNumberChild(n:Pair): Option[Regular] = {
    n.x match {
      case p: Regular => Some(p)
      case p: Pair => leftNumberChild(p)
    }
  }

  def pairToExplode(n:Pair, currentDepth: Int): Option[Pair] = {
    //println(s"$n, $currentDepth")
    // check x
    var result = n.x match {
      case p: Pair => {
        if (currentDepth == 4)
          Some(p)
        else {
          pairToExplode(p, currentDepth + 1)
        }
      }
      case r: Regular => None
    }
    if (result.isEmpty) {
      result = n.y match {
        case p: Pair => {
          if (currentDepth == 4)
            Some(p)
          else {
            pairToExplode(p, currentDepth + 1)
          }
        }
        case r: Regular => None
      }
    }
    result
  }

  // parse a pair, where iterator is inside the pair
  def parsePair(s: Iterator[Char]): Pair = {
    val x = parseNumber(s)
    assert(s.next() == ',')
    val y = parseNumber(s)
    assert(s.next() == ']')
    Pair(x, y)
  }

  def parseNumber(s: Iterator[Char]):SnailFishNumber = {
    val char = s.next()
    char match {
      case '[' => parsePair(s)
      case n => Regular(Integer.parseInt(n.toString, 10))
    }
  }
}

trait SnailFishNumber {
  def magnitude: Long
  var parent: Option[Pair] = None
}

case class Pair(var x:SnailFishNumber, var y:SnailFishNumber) extends SnailFishNumber {
  x.parent = Some(this)
  y.parent = Some(this)

  override def magnitude: Long = {
    (3 * x.magnitude) + (2 * y.magnitude)
  }
  override def toString() = {
    s"[$x,$y]"
  }
}
case class Regular(var value:Int) extends SnailFishNumber {
  override def magnitude: Long = value
  override def toString() = {
    value.toString
  }
}
