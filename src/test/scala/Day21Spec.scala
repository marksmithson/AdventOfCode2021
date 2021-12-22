package me.smithson

class Day21Spec extends DaySpec {
  override def part1Result(): String = "739785"

  override def part2Result(): String = "444356092776315"

  override def day(): Day = Day21

  "First Die roll" should "return rolled die and 1" in {
    val d = Die(0)
    val (r, d1) = d.roll()

    assert (r == 1)
    assert (d1.rolls == 1)
  }

  "100th Die roll" should "return rolled die and 100" in {
    var (r,d) = (0, Die(0))
    (1 to 100).foreach(_=>{
      d.roll() match {
        case (a,b) => {
          r = a
          d = b
        }
      }
    })

    assert (r == 100)
    assert (d.rolls == 100)
  }

  "101st Die roll" should "return rolled die and 1" in {
    var (r,d) = (0, Die(0))
    (1 to 101).foreach(_=>{
      d.roll() match {
        case (a,b) => {
          r = a
          d = b
        }
      }
    })

    assert (r == 1)
    assert (d.rolls == 101)
  }

  "move 4,5" should "return 9" in {
    val r = Day21.move(4,5)
    assert(r == 9)
  }
  "move 4,6" should "return 10" in {
    val r = Day21.move(4,6)
    assert(r == 10)
  }
  "move 4,7" should "return 1" in {
    val r = Day21.move(4,7)
    assert(r == 1)
  }
  "move 14,7" should "return 1" in {
    val r = Day21.move(4,7)
    assert(r == 1)
  }
}
