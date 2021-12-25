package me.smithson

import Day22.Cube

class Day22Spec extends DaySpec {
  override def part1Result(): String = "590784"

  override def part2Result(): String = "39769202357779"

  override def day(): Day = Day22

  "parseOperation" should "return operation" in {
    val input = "on x=-20..26,y=-36..17,z=-47..7"
    val op = Day22.parseOperation(input)

    assert (op.on == true)
    assert (op.xRange.head == -20)
    assert (op.xRange.last == 26)
    assert (op.yRange.head == -36)
    assert (op.yRange.last == 17)
    assert (op.zRange.head == -47)
    assert (op.zRange.last == 7)
  }

  "intersects" should "work for empty ranges" in {
    val op = Day22.parseOperation("on x=10..12,y=10..12,z=10..12")
    val range = (-10 to -10)
    val result = op.intersects(op.xRange,op.yRange,range)
    assert (!result)
  }

  "applyOp" should "turn on 27 cubes" in {
    val op = Day22.parseOperation("on x=10..12,y=10..12,z=10..12")
    val cubeSize = 100
    val cube = Array.fill[Array[Array[Boolean]]](cubeSize+1)(
      Array.fill[Array[Boolean]](cubeSize+1)(
        Array.fill[Boolean](cubeSize+1)(false)
      )
    )
    val result = Day22.applyOp(op, cube, 50)

    val count = result.flatten.flatten.count(_==true)
    assert(count == 27)

  }

  "applyOp" should "turn on 8 cubes when outside of cube" in {
    val op = Day22.parseOperation("on x=10..12,y=10..12,z=10..12")
    val cubeSize = 11
    val cube = Array.fill[Array[Array[Boolean]]](cubeSize+1)(
      Array.fill[Array[Boolean]](cubeSize+1)(
        Array.fill[Boolean](cubeSize+1)(false)
      )
    )
    val result = Day22.applyOp(op, cube, 0)

    val count = result.flatten.flatten.count(_==true)
    assert(count == 8)
  }

  "rangeRemove" should "work as expected" in {
    val r = (0 to 10)

    assert(Day22.rangeRemove(r, (0 to 10)) == List())
    assert(Day22.rangeRemove(r, (-10 to 20)) == List())
    assert(Day22.rangeRemove(r, (0 to 5)) == List((6 to 10)))
    assert(Day22.rangeRemove(r, (-10 to 5)) == List((6 to 10)))
    assert(Day22.rangeRemove(r, (5 to 10)) == List((0 to 4)))
    assert(Day22.rangeRemove(r, (5 to 20)) == List((0 to 4)))
    assert(Day22.rangeRemove(r, (5 to 8)) == List((0 to 4), (9 to 10)))
  }

  "removeFromCube" should "removeCube inside" in {
    val r = (0 to 10)
    val r1 = (4 to 6)
    val c = Cube(r,r,r)
    val remove = Cube(r1,r1,r1)
    val removed = c.remove(remove)
    println(removed)
    assert(removed.length == 6)
  }

  "removeFromCube" should "removeCube on edge" in {
    val r = (0 to 10)
    val r1 = (4 to 20)
    val c = Cube(r,r,r)
    val remove = Cube(r1,r1,r1)
    val removed = c.remove(remove)
    println(removed)
    assert(removed.length == 3)
  }


  "part2" should "be correct for alternative data" in {
      val altData = DataLoader.Load(s"day22-test-alt")
      val result = Day22.Part2(altData)
      assert(result == "2758514936282235")
  }
}
