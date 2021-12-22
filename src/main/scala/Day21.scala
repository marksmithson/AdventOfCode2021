package me.smithson

object Day21 extends Day {
  override def Part1(data: Iterator[String]): String = {
    var (p1,p2) = parseData(data)
    var s1 = 0
    var s2 = 0
    var die = Die(0)
    var round = 0
    while (s1 < 1000 && s2 < 1000) {
      val (r, d1) = die.rollTimes(3)
      die = d1
      if(round % 2 == 0) {
        p1 = move(p1, r.sum)
        s1 = s1 + p1
      }
      else {
        p2 = move(p2, r.sum)
        s2 = s2 + p2
      }
      round = round + 1
    }
    val loser = if (s1 >= 1000) s2 else s1
    (loser.toLong * die.rolls).toString
  }

  def move(start: Int, delta: Int) = {
    val r = (start + delta) % 10
    if (r == 0) 10 else r
  }

  val winningScore = 21
  override def Part2(data: Iterator[String]): String = {
    val (p1,p2) = parseData(data)

    val (p1winCount, p2winCount) = playGame(PlayerState(p1,0), PlayerState(p2,0),1,1)

    Math.max(p1winCount, p2winCount).toString
  }

  def playGame(p1: PlayerState, p2:PlayerState, round:Int, universes:Long): (Long, Long) = {
    (3 to 9).map(roll => {
      val ps = if (round % 2 == 0){
        val p2p = move(p2.position, roll)
        PlayerState(p2p, p2.score + p2p)
      } else {
        val p1p = move(p1.position, roll)
        PlayerState(p1p, p1.score + p1p)
      }

      val u = universes * universesForRollValue(roll)

      if (ps.score >= winningScore) {
        if (round % 2 == 0) (0L, u) else (u, 0L)
      }
      else {
        val (ps1, ps2) = if (round % 2 == 0) (p1, ps) else (ps, p2)
        playGame(ps1, ps2, round + 1, u)
      }
    }).reduce((a,b) => {(a._1 + b._1, a._2 + b._2)})
  }

  def universesForRollValue(value:Int) = {
    value match {
      case 3 => 1
      case 4 => 3
      case 5 => 6
      case 6 => 7
      case 7 => 6
      case 8 => 3
      case 9 => 1
    }
  }

  def parseData(data: Iterator[String]) = {
    val starts = data.take(2).map(r => Integer.parseInt(r.substring(r.indexOf(":") + 1).trim))
    (starts.next(), starts.next())
  }
}

case class Die(rolls:Int) {
  def roll(): (Int, Die) = {
    val result = rolls % 100 + 1
    (result, Die(rolls+1))
  }

  def rollTimes(times:Int):(Array[Int], Die) = {
    var d = this
    val rollResult = (1 to times).map(_ => {
      val (r, d1) = d.roll()
      d = d1
      r
    }).toArray
    (rollResult, d)
  }
}

case class PlayerState(position:Int, score:Int)
