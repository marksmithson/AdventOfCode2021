package me.smithson

object Day20 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val (algo, image) = parseData(data)

    val enhanced = enhanceN(image, algo, 2)

    enhanced.flatten.count(c => c == '#').toString
  }

  override def Part2(data: Iterator[String]): String = {
    val (algo, image) = parseData(data)

    val enhanced = enhanceN(image, algo, 50)

    enhanced.flatten.count(c => c == '#').toString
  }

  def enhanceN(image:Array[Array[Char]], algo: Array[Char],times:Int) = {
    var enhanced: Array[Array[Char]] = image
    val toggleInfinite = algo(0) == '#' // will cause infinite image chars to toggle between 0 and 1
    (0 until times).foreach( ix => {
        val infinite = if (toggleInfinite && ix % 2 == 1) "1" else "0"
        enhanced = enhance(enhanced, algo, infinite)
      }
    )
    enhanced
  }
  def enhance(image:Array[Array[Char]], algo: Array[Char], infinite:String): Array[Array[Char]] = {
    val width = image(0).length
    val height = image.length

    (-1 to height).map(y =>
      (-1 to width).map(x =>
        algo(pixelEnhancementIndex(image,x,y, infinite))
      ).toArray
    ).toArray
  }

  def pixelEnhancementIndex(image:Array[Array[Char]], x: Int, y:Int, infinite:String) = {
    val sb = new StringBuilder
    val width = image(0).length
    (y-1 to y+1).foreach(cy => {
      (x-1 to x+1).foreach(cx => {
        if (cy < 0 || cx < 0 || cy >= image.length || cx >= width) {
          sb.append(infinite)
        }
        else {
          sb.append(if (image(cy)(cx) == '#') "1" else "0")
        }
      })
    })
    Integer.parseInt(sb.toString(), 2)
  }

  def parseData(data: Iterator[String]) = {
    val algo = data.next().toCharArray

    // read empty row
    data.next()

    val image = data.map(r => r.toCharArray).toArray

    (algo, image)
  }

  def printImage(image:Array[Array[Char]]) = {
    image.foreach(r => println(r.mkString))
  }
}
