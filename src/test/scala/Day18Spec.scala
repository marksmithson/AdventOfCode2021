package me.smithson

import org.scalatest.prop.TableDrivenPropertyChecks

class Day18Spec extends DaySpec with TableDrivenPropertyChecks {
  override def part1Result(): String = "4140"

  override def part2Result(): String = "3993"

  override def day(): Day = Day18

  "parseNumber for regulars" should "return single pair" in {
    val input = "[0,8]".toCharArray.iterator

    val result = Day18.parseNumber(input)

    assert (result == Pair(Regular(0), Regular(8)))
  }

  "parseNumber for nested" should "return nested pairs" in {
    val input = "[[[1,2],0],8]".toCharArray.iterator

    val result = Day18.parseNumber(input)

    assert (result == Pair(Pair(Pair(Regular(1), Regular(2)),Regular(0)),Regular(8)))
  }

  "parseNumber for nested to right" should "return nested pairs" in {
    val input = "[[0,[1,2]],8]".toCharArray.iterator

    val result = Day18.parseNumber(input)

    assert (result == Pair(Pair(Regular(0),Pair(Regular(1), Regular(2))),Regular(8)))
  }

  val explodeChecks = Table(("initial", "result"),
    ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
    ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
    ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
    ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
    ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
  )
  forAll(explodeChecks) { (initial: String, expected: String) =>
    "explode" should s"be $expected for $initial" in {
      val num = Day18.parseNumber(initial.toCharArray.iterator)
      val e = Day18.parseNumber(expected.toCharArray.iterator)

      val r = Day18.explode(num.asInstanceOf[Pair])
      assert(r)
      assert(num == e)
    }
  }

  "ex temp" should "x" in {
    val initial = "[[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]],[[[[4,2],2],6],[8,7]]]"
    val expected = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    //val initial = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    //val expected = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    val num = Day18.parseNumber(initial.toCharArray.iterator)

    val e = Day18.parseNumber(expected.toCharArray.iterator)

    val r = Day18.reduce(num.asInstanceOf[Pair])
    assert(num == e)
  }

  "split[[15,1],0]" should "be [[[7,8],1],0]" in {
    val num = Pair(Pair(Regular(15), Regular(1)), Regular(0))
    val e = Pair(Pair(Pair(Regular(7), Regular(8)), Regular(1)), Regular(0))

    val r = Day18.split(num)
    assert(r)
    assert(num == e)
  }
  val addChecks = Table(("a", "b", "expected"),
    ("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
    ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]", "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"),
    ("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]", "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]", "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"),
    ("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]", "[[[[4,2],2],6],[8,7]]", "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
  )
  forAll(addChecks) { (a: String, b: String, expected: String) => {
    s"add $a + $b" should s"be $expected" in {
      println(s"add $a + $b")
      val x = Day18.parseNumber(a.toCharArray.iterator)
      val y = Day18.parseNumber(b.toCharArray.iterator)
      val e = Day18.parseNumber(expected.toCharArray.iterator)

      val r = Day18.add(x, y)
      assert(r == e)
    }
  }
  }

  "magnitude of [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" should "be 3488" in {
    val num = Day18.parseNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]".toCharArray.iterator)
    assert (num.magnitude == 3488)
  }
}
