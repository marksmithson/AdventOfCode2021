package me.smithson
import scala.collection.Map

object Day6 extends Day {
  def Part1(data: Iterator[String]) = {
    val fish = parseData(data)
    runGenerations(fish, 80).length.toString
  }

  /**
   * Result is likely to be more than a 32 bit integer
   *
   * We also need compress the array as this will be a lot of data.
   *
   * So instead of a single number we will have an array of ages 0-8, with a count of fish of that age
   * @return
   */
  def Part2(data: Iterator[String]) = {
    val fish = parseData(data)
    val fishAges = buildFishAges(fish)
    runAgeGenerations(fishAges, 256).values.sum.toString
  }

  def buildFishAges(fish: Array[Int]) = {
    val fishAges = fish.groupBy(identity).map({ case (key, value) => (key, value.length)})
    (0 to 8).map(age => {
      val count = fishAges.get(age)
      count match {
        case None => age -> 0.toLong
        case Some(ageCount) => age -> ageCount.toLong
      }
    }).toMap
  }

  def runAgeGenerations(fishAges:Map[Int,Long], generations: Int) = {
    var result = fishAges
    (1 to generations).foreach(_ => {
      result = runAgeGeneration(result)
    })
    result
  }

  def runAgeGeneration(fishAges:Map[Int,Long]) = {
    val newFish = fishAges.get(0).get
    fishAges.map({
      case (key,_) if (key == 0) => 8 -> newFish
      case (key,value) if (key == 7) => 6 -> (value + newFish)
      case (key,value) => key - 1 -> value
    })
  }

  def runGenerations(fish: Array[Int], generations: Int) = {
    var result = fish
    (1 to generations).foreach(_ => {
      result = fishNextDay(result)
    })
    result
  }

  def parseData(data: Iterator[String]) = {
    val fish = data.map(_.split(",").map(Integer.parseInt))
    fish.next()
  }

  def fishNextDay(data: Array[Int]) = {
    val existingFish = data.map(age => if (age == 0) 6 else age -1)
    val newFish = data.filter(age => age == 0).map(_ => 8)
    existingFish ++ newFish
  }
}
