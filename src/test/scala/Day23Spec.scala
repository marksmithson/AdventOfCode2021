package me.smithson

class Day23Spec extends DaySpec {
  override def part1Result(): String = "12521"

  override def part2Result(): String = "44169"

  override def day(): Day = Day23

  "hallway blocked" should "be false for adjacent positions" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val locations = Day23.Locations(hallway = Array.fill(7)('A'), rooms)

    assert(!locations.isHallwayBlocked(1,0))
    assert(!locations.isHallwayBlocked(2,0))
    assert(!locations.isHallwayBlocked(2,1))
    assert(!locations.isHallwayBlocked(3,1))
    assert(!locations.isHallwayBlocked(3,2))
    assert(!locations.isHallwayBlocked(4,2))
    assert(!locations.isHallwayBlocked(4,3))
    assert(!locations.isHallwayBlocked(5,3))
  }

  "hallway blocked" should "next to room" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val hallway = Array.fill(7)(' ')
    hallway(2) = 'A'
    val locations = Day23.Locations(hallway, rooms)

    assert(locations.isHallwayBlocked(0,1))
    assert(!locations.isHallwayBlocked(1,0))
    assert(locations.isHallwayBlocked(0,3))
    assert(!locations.isHallwayBlocked(0,0))
    assert(locations.isHallwayBlocked(3,0))
    assert(!locations.isHallwayBlocked(3,1))
    assert(!locations.isHallwayBlocked(3,2))
  }

  "hallway blocked" should "next to room - right" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val hallway = Array.fill(7)(' ')
    hallway(4) = 'A'
    val locations = Day23.Locations(hallway, rooms)

    assert(locations.isHallwayBlocked(6,2))
    assert(!locations.isHallwayBlocked(6,3))
    assert(!locations.isHallwayBlocked(5,3))
    assert(!locations.isHallwayBlocked(3,2))
    assert(locations.isHallwayBlocked(3,3))
  }

  "hallway blocked" should "be true for non-adjacent positions when full" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val locations = Day23.Locations(hallway = Array.fill(7)('A'), rooms)

    assert(locations.isHallwayBlocked(0,0))
    assert(locations.isHallwayBlocked(7,0))
    assert(locations.isHallwayBlocked(0,1))
    assert(locations.isHallwayBlocked(7,1))
    assert(locations.isHallwayBlocked(0,2))
    assert(locations.isHallwayBlocked(7,2))
    assert(locations.isHallwayBlocked(0,3))
    assert(locations.isHallwayBlocked(7,3))
  }

  "hallway blocked" should "be false for non-adjacent positions when empty" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val locations = Day23.Locations(hallway = Array.fill(7)(' '), rooms)

    assert(!locations.isHallwayBlocked(0,0))
    assert(!locations.isHallwayBlocked(7,0))
    assert(!locations.isHallwayBlocked(0,1))
    assert(!locations.isHallwayBlocked(7,1))
    assert(!locations.isHallwayBlocked(0,2))
    assert(!locations.isHallwayBlocked(7,2))
    assert(!locations.isHallwayBlocked(0,3))
    assert(!locations.isHallwayBlocked(7,3))
  }

  "movesToRoom" should "be correct" in {
    val rooms:Array[Array[Char]] = Array.fill(4)(Array.fill(2)(' '))
    val locations = Day23.Locations(hallway = Array.fill(7)(' '), rooms)

    assert(locations.movesToRoom(0,0) == 2)
    assert(locations.movesToRoom(1,0) == 1)
    assert(locations.movesToRoom(2,1) == 1)
    assert(locations.movesToRoom(0,1) == 4)
    assert(locations.movesToRoom(0,2) == 6)
    assert(locations.movesToRoom(1,2) == 5)
    assert(locations.movesToRoom(3,3) == 3)

    assert(locations.movesToRoom(6,3) == 2)
    assert(locations.movesToRoom(6,2) == 4)
  }
}
