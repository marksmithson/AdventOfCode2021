package me.smithson

object Day19 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val scanners = parseData(data)
    val points = allPoints(scanners)
    points.length.toString
  }

  override def Part2(data: Iterator[String]): String = {
    val scanners = parseData(data)
    val origin = Point3D(Array(0,0,0))
    addCoordTransforms(scanners)
    val origins = scanners.map(s => {
      transformPoint(origin, s.coordTranslation.get)
    })
    origins.flatMap(o1 => {
      origins.map(o2 => {
        manhattenDistance(o1,o2)
      })
    }).max.toString
  }

  def manhattenDistance(p1:Point3D, p2:Point3D) = {
    Math.abs(p1.coordinates(0) - p2.coordinates(0)) +
      Math.abs(p1.coordinates(1) - p2.coordinates(1)) +
      Math.abs(p1.coordinates(2) - p2.coordinates(2))
  }


  def addCoordTransforms(scanners: List[Scanner]) = {
    val rootScanner = scanners.head
    rootScanner.coordTranslation = Some(Array(
      Array(1,0,0,0),
      Array(0,1,0,0),
      Array(0,0,1,0),
      Array(0,0,0,1),
    ))

    var mappedScanners = List(scanners.head)

    var unmappedScanners = scanners.tail
    while(unmappedScanners.nonEmpty) {
      unmappedScanners.foreach(us => {
        // is there a better way to do this - find will short circuit - better than a map
        // but we have to run coordinateTransform again to get the translation matrix
        val mapped = mappedScanners.find(ms => {
          coordinateTransform(ms, us).isDefined
        })
        if (mapped.isDefined) {
          us.coordTranslation = Some(matrixProduct(mapped.get.coordTranslation.get, coordinateTransform(mapped.get, us).get))
          mappedScanners = us :: mappedScanners
        }
      })
      unmappedScanners = scanners.filter(s => s.coordTranslation.isEmpty)
    }
    assert(mappedScanners.length == scanners.length)

    /*
       def printcoords(coords: List[Point3D]) = {
        println ("=====")
        coords.sortBy(p=>p.coordinates(0)).foreach(p => {
          println(p.coordinates.mkString(","))
        })
        println ("=====")
      }

        val s1 = scanners(1)
        val tfm1 = coordinateTransform(rootScanner, s1).get
        val s1p = transform(s1.coordinates, tfm1)
        val i1 = rootScanner.coordinates.intersect(s1p)
        //printcoords(i1) - correct

        val s4 = scanners(4)
        val tfm4 = s4.coordTranslation.get
        val s4p = transform(s4.coordinates, tfm4)
        val i4 = s1p.intersect(s4p)
        //printcoords(i4)
    */
    // build a list of all points in the same coordinate system and get a distinct list


  }
  def allPoints(scanners:List[Scanner]) = {
    addCoordTransforms(scanners)

    scanners.flatMap(s => {
      transform(s.coordinates, s.coordTranslation.get)
    }).distinct
  }

  def matrixProduct(m1:Array[Array[Int]], m2:Array[Array[Int]]) = {
    // m1 may be 4x4 matrix and m2 a 1x3 (for a rotation and translation)
    // in this case we need it to be a 1x4 and when we return, strip the last row
    val m2FixUp = m1(0).length == m2.length + 1 && m2(0).length == 1
    val im2:Array[Array[Int]] = if (m2FixUp) m2 :+ Array(1) else m2

    val m2cols = colToRow(im2) // make it easier to process
    val r = m1.map(m1r =>
      m2cols.map(m2c =>
        m1r.zipWithIndex.map({case ( m1Cell, ix ) =>
          m1Cell * m2c(ix)
        }).sum
      )
    )
    if (m2FixUp) r.dropRight(1) else r
  }

  /*
    translates columns to rows
   */
  def colToRow(m:Array[Array[Int]]) = {
    m(0).indices.map(col => {
      m.map(row => row(col))
    }).toArray
  }

  def rotationTransform(axis:Int, quarters: Int) = {
    rotationTransforms(axis)(quarters * 90).map(r => r.map(c=>c.toInt))
  }

  val rotationTransforms = Array(xRotationTransform, yRotationTransform, zRotationTransform)
  /**
   * Get a transform matrix for x rotation
   */
  val xRotationTransform:Int => Array[Array[Double]] = (angle:Int) => {
    val a = Math.toRadians(angle)
    Array(
      Array(1.0,0.0,0.0),
      Array(0.0, Math.cos(a), 0.0 - Math.sin(a)),
      Array(0.0, Math.sin(a), Math.cos(a))
    )
  }
  val yRotationTransform:Int => Array[Array[Double]] = (angle:Int) => {
    val a = Math.toRadians(angle)
    Array(
      Array(Math.cos(a), 0.0, Math.sin(a)),
      Array(0.0, 1.0, 0.0),
      Array(0.0 - Math.sin(a), 0.0, Math.cos(a))
    )
  }
  val zRotationTransform:Int => Array[Array[Double]] = (angle:Int) => {
    val a = Math.toRadians(angle)
    Array(
      Array(Math.cos(a), 0.0 - Math.sin(a), 0.0),
      Array(Math.sin(a), Math.cos(a), 0.0),
      Array(0.0, 0.0, 1.0)
    )
  }


  def coordinateTransform(s1: Scanner, s2:Scanner) = {
    val tr = transformList().find(t => {
      translation(s1.coordinates, transform(s2.coordinates, t)).isDefined
    })

    tr match {
      case Some(t) => {
        val translate = translation(s1.coordinates, transform(s2.coordinates, t)).get
        // we can add the translation to the matrix - needs to be a 4x4 matrix
        val shift = Array (
          t(0) :+ translate.x,
          t(1) :+ translate.y,
          t(2) :+ translate.z,
          Array(0,0,0,1)
        )
        Some(shift)
      }
      case None => None
    }
  }

  def translation(s1: List[Point3D], s2: List[Point3D]): Option[Delta] = {
    // work out if there are at least 12 overlapping points
    val deltas = s1.flatMap(p1 => {
      s2.map(p2 => {
          Delta(
            p1.coordinates(0) - p2.coordinates(0),
            p1.coordinates(1) - p2.coordinates(1),
            p1.coordinates(2) - p2.coordinates(2),
          )
      })
    })

    val matchedDelta = deltas.groupBy(d => (d)).find({case(_,items) => items.length >=12 })

    matchedDelta match {
      case Some((delta,_)) => Some(delta)
      case None => None
    }
  }

  def axisRotationTransforms(axis:Int) = {
    (0 to 3).map(quarter => {
      rotationTransform(axis, quarter)
    })
  }

  def transformList() = {
    var transforms = axisRotationTransforms(2)

    // z down - rotate around x 180 degrees
    var orientation = rotationTransform(0,2)
    transforms = transforms ++ axisRotationTransforms(2).map(t => matrixProduct(t, orientation))

    orientation = rotationTransform(0,1)
    transforms = transforms ++ axisRotationTransforms(1).map(t => matrixProduct(t, orientation))
    orientation = rotationTransform(0,3)
    transforms = transforms ++ axisRotationTransforms(1).map(t => matrixProduct(t, orientation))

    orientation = rotationTransform(1,1)
    transforms = transforms ++ axisRotationTransforms(0).map(t => matrixProduct(t, orientation))
    orientation = rotationTransform(1,3)
    transforms = transforms ++ axisRotationTransforms(0).map(t => matrixProduct(t, orientation))
    transforms
  }

  def transform(points:List[Point3D], t: Array[Array[Int]]) = {
    points.map(p => {
      transformPoint(p, t)
    })
  }

  def transformPoint(p:Point3D, t: Array[Array[Int]]) = {
    val c = colToRow(Array(p.coordinates))
    val tc = colToRow(matrixProduct(t, c))
    Point3D(tc(0))
  }

  def parseData(data: Iterator[String]) = {
    var result: List[Scanner] = List()
    while(data.hasNext) {
      result = parseScanner(data) :: result
    }
    result.reverse
  }

  def parseScanner(data: Iterator[String]) = {
    val header = data.next()
    val index = Integer.parseInt(
      header.substring(12).split(" ")(0)
    )
    Scanner(index, readCoordinates(data))
  }

  def readCoordinates(data: Iterator[String]) = {
    data.takeWhile(line => line != "").map(line => {
      val coordinates = line.split(",").map(Integer.parseInt)
      Point3D(coordinates)
    }).toList
  }

  def printTransform(t:Array[Array[Int]]) = {
    println("-----")
    t.foreach(r => println(r.mkString(",")))
    println("-----")
    println
  }
}
case class Scanner(index:Int, coordinates: List[Point3D]){
  var coordTranslation: Option[Array[Array[Int]]] = None
}

case class Point3D( coordinates: Array[Int]){ // use an array to make maths easier
  // equals and hashcode for distinct etc operations
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Point3D] && obj.asInstanceOf[Point3D].coordinates.sameElements(coordinates)
  }

  override def hashCode(): Int = {
    coordinates.map(i => i.hashCode()).sum
  }
}
case class Delta( x: Int, y:Int, z:Int )
