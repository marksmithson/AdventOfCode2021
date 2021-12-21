package me.smithson
import org.scalatest.matchers.should._

class Day19Spec extends DaySpec with Matchers {
  override def part1Result(): String = "79"

  override def part2Result(): String = "3621"

  override def day(): Day = Day19

  "matrixProduct" should "multiply" in {
    val m1 = Array(
      Array(1,2,3),
      Array(4,5,6)
    )
    val m2 = Array(
      Array(7,8),
      Array(9,10),
      Array(11,12)
    )
    val expected = Array(
      Array(58,64),
      Array(139,154)
    )

    val result = Day19.matrixProduct(m1, m2)
    result should equal (expected)
  }

  "translate matrixProduct" should "translate" in {
    var m1 = Array(
      Array(1,0,0,10),
      Array(0,1,0,20),
      Array(0,0,1,30),
      Array(0,0,0,1),
    )
    var m2 = Array(Array(1), Array(2), Array(3))
    var expected = Array(Array(11), Array(22), Array(33))
    val result = Day19.matrixProduct(m1, m2)
    result should equal (expected)
  }

  // prove we can combine transforms and still get the same output
  "combine translations using matrixProduct" should "translate" in {
    var m1a = Array(
      Array(0,-1,0,10),
      Array(1,0,0,20),
      Array(0,0,1,30),
      Array(0,0,0,1),
    )
    var m1b = Array(
      Array(0,-1,0,-5),
      Array(1,0,0,-10),
      Array(0,0,1,-20),
      Array(0,0,0,1),
    )

    val m2 = Array(Array(1), Array(2), Array(3))

    val s1r = Day19.matrixProduct(m1a, m2)
    printTransform(s1r)
    val expected = Day19.matrixProduct(m1b, s1r)
    printTransform(expected)

    val m1 = Day19.matrixProduct(m1b, m1a)
    val result = Day19.matrixProduct(m1, m2)
    printTransform(result)
    printTransform(m1)
    result should equal (expected)
  }

  "rotationTransform" should "be OK" in {
    val t = Day19.rotationTransform(2, 1)

    t should equal (Array(
        Array(0,-1,0),
        Array(1,0,0),
        Array(0,0,1)
      )
    )
  }

  "transform list" should "be 24 length" in {
    val transforms = Day19.transformList()
    // print these out - shows no duplicates ... thought there would be...
    transforms.sortBy(t => t(0).mkString(",") + t(1).mkString(",")).foreach(t => {
      printTransform(t)
    })
    transforms.length should be (24)
  }

  def printTransform(t:Array[Array[Int]]) = {
    println("-----")
    t.foreach(r => println(r.mkString(",")))
    println("-----")
    println
  }

  "transform point" should "be rotation" in {
    val t = Day19.rotationTransform(2, 1)
    val p = Point3D(Array(1,1,1))
    val r = Day19.transformPoint(p, t)

    r.coordinates should equal (Array(-1,1,1))
  }

  "transform point" should "be composite rotation" in {
    val t0 = Day19.rotationTransform(2, 1)
    val t1 = Day19.rotationTransform(0, 1)
    val t = Day19.matrixProduct(t1, t0)
    val p = Point3D(Array(1,1,1))
    val r = Day19.transformPoint(p, t)

    r.coordinates should equal (Array(-1,-1, 1))
  }

  "manhatten distance" should "be 3621" in {
    val p1 = Point3D(Array(1105,-1205,1229))
    val p2 = Point3D(Array(-92,-2380,-20))

    val r = Day19.manhattenDistance(p1, p2)

    assert (r == 3621)
  }
}
