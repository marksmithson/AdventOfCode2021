package me.smithson

object Day22 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val ops = parseData(data).toList

    // offset by 50 to deal with negative numbers in output cube
    val offset = 50
    val cubeSize = 100
    val cube = Array.fill[Array[Array[Boolean]]](cubeSize + 1)(
      Array.fill[Array[Boolean]](cubeSize + 1)(
        Array.fill[Boolean](cubeSize + 1)(false)
      )
    )

    var result:Array[Array[Array[Boolean]]] = cube
    ops.foreach(op => {
      result = applyOp(op, result, offset)
    })

    result.flatten.flatten.count(_ == true).toString
  }


  def applyOp(op:Operation, c:Array[Array[Array[Boolean]]], offset:Int) = {
    val cubeDimension = (0 - offset to c.length)
    if (op.intersects(cubeDimension, cubeDimension, cubeDimension)) {
      c.indices.map(x => {
        c.indices.map(y => {
          c.indices.map(z => {
            if (op.xRange.contains(x - offset) && op.yRange.contains(y - offset) && op.zRange.contains(z - offset)) {
              op.on
            }
            else {
              c(x)(y)(z)
            }
          }).toArray
        }).toArray
      }).toArray
    }
    else {
      c
    }
  }

  def intersection(r1:Range, r2:Range) = {
    val seq = r1.intersect(r2)
    (seq.min to seq.max)
  }

  def rangesIntersect(r1: Range, r2:Range) = {
    (r2.min >= r1.min && r2.min <= r1.max)  || // min between
      (r2.max >= r1.min && r2.max <= r1.max) || // max between
      (r2.min <= r1.min && r2.max >= r1.max) // min and max either side
  }

  override def Part2(data: Iterator[String]): String = {
    val ops = parseData(data).toList

    var onCubes:List[Cube] = List()

    ops.foreach(op => {
      val opCube = Cube(op.xRange, op.yRange, op.zRange)
      if (!op.on) {
        onCubes = onCubes.flatMap(c => c.remove(opCube))
      }
      else {
        onCubes = opCube :: onCubes.flatMap(c => c.remove(opCube))
      }
    })

    onCubes.map(c => c.xRange.length.toLong * c.yRange.length.toLong * c.zRange.length.toLong).sum.toString
  }

  def parseData(data: Iterator[String]) = {
    data.map(r=> {
      parseOperation(r)
    })
  }

  def parseOperation(str: String) = {
    val parts = str.split(" ")
    val op = parts.head
    val rngs = parts(1).split(",")
    val ranges = rngs.map(sr => {
      sr.split("=")(1).split("\\.\\.").map(Integer.parseInt)
    })
    Operation(op == "on",
      (ranges(0)(0) to ranges(0)(1)),
      (ranges(1)(0) to ranges(1)(1)),
      (ranges(2)(0) to ranges(2)(1)),
    )
  }

  def rangeRemove(r1:Range, r2:Range) = {
    val int = r1.intersect(r2)
    (int.min, int.max) match {
      case (m,x) if m == r1.min && x == r1.max => List()
      case (m,x) if m == r1.min && x < r1.max => List((x + 1 to r1.max))
      case (m,x) if m > r1.min && x == r1.max => List((r1.min until m))
      case (m,x) if m > r1.min && x < r1.max => List((r1.min until m), (x + 1 to r1.max))
    }
  }

  case class Cube (xRange: Range, yRange: Range, zRange:Range) {

    def remove(toRemove:Cube): List[Cube] = {
      if (rangesIntersect(toRemove.xRange, xRange) && rangesIntersect(toRemove.yRange, yRange) && rangesIntersect(toRemove.zRange, zRange)){
        val xIntersect = intersection(xRange, toRemove.xRange)
        val yIntersect = intersection(yRange, toRemove.yRange)

        rangeRemove(xRange, toRemove.xRange).map(x => { // left and right of hole
          Cube(x, yRange, zRange)
        }) ++
        rangeRemove(yRange, toRemove.yRange).map(y => { // front and back
          Cube(xIntersect, y, zRange)
        }) ++
        rangeRemove(zRange, toRemove.zRange).map(z => { // top and bottom
          Cube(xIntersect, yIntersect, z)
        })
      }
      else {
        List(this)
      }
    }
  }
}

case class Operation (on:Boolean, xRange:Range, yRange:Range, zRange:Range) {
  def intersects(x:Range, y:Range, z:Range) = {
    xRange.intersect(x).nonEmpty && yRange.intersect(y).nonEmpty && zRange.intersect(z).nonEmpty
  }
  def affects(x:Int, y:Int, z:Int) = {
    xRange.contains(x) && yRange.contains(y) && zRange.contains(z)
  }
}
