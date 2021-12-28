package me.smithson

object Day23 extends Day {
  override def Part1(data: Iterator[String]): String = {
    val rooms = parseData(data)
    // the moves are constrained
    // only top level letters can move, or letters in corridor where room is free.
    // top level letters can move into a hallway slot that is no occupied
    val locations = Locations(hallway = Array.fill(7)(' '), rooms)

    locations.minCost().toString

  }

  val charOffset = 'A'.toInt

  def podCost(pod:Char) = {
    val power = pod.toInt - charOffset
    Math.pow(10, power).toInt
  }

  override def Part2(data: Iterator[String]): String = {
    val originalRooms = parseData(data)
    val additional = Array(Array('D','D'), Array('C','B'), Array('B','A'), Array('A','C'))
    val rooms = originalRooms.zip(additional).map({case (o,e)=> Array(o(0), e(0), e(1), o(1))})

    val locations = Locations(hallway = Array.fill(7)(' '), rooms)

    locations.minCost().toString
  }

  def parseData(data:Iterator[String]) = {
    // ignore first 2 lines as these are the corridor
    data.take(2).foreach(_=>{}) // need for each to consume the rows
    val lines = data.take(2).map(line => {
      line.substring(3,10).split("\\#").map(c=>c.charAt(0))
    }).toArray
    (0 to 3).map(ix => Array(lines(0)(ix), lines(1)(ix))).toArray
  }

  case class Move (result:Locations, cost:Int)
  val locationsCache:scala.collection.mutable.HashMap[String, Int] = scala.collection.mutable.HashMap[String, Int]()

  /**
   * Hallway is an array of 7
   * Rooms is an array of 4 rooms of 2 chars
   *
   * H: 0 1 2 3 4 5 6
   * R:    0 1 2 3
   *
   * @param hallway
   * @param rooms
   */
  case class Locations(hallway:Array[Char], rooms:Array[Array[Char]]) {

    /**
     * Can we move from hallway from to a room
     */
    def isHallwayBlocked(from:Int, roomIndex:Int):Boolean = {
      if (from == roomIndex + 2 || from == roomIndex + 1) { // can't be blocked on side positions
        false
      }
      if (from < roomIndex + 1){
        !hallway.slice(from + 1, roomIndex + 2).forall(c => c == ' ')
      }
      else {
        !hallway.slice(roomIndex + 2, from).forall(c => c == ' ')
      }
    }

    /**
     * Can move into room when the char matches the room and
     *  - the room is empty OR
     *  - the room has one space and the other is the roomChar
     */
    def canMoveIntoRoom(room: Array[Char], roomChar:Char) = {
      val spaces = room.count(_ == ' ')
      val filling = room.forall(c => {c  == ' ' || c == roomChar})
      spaces == room.length || (filling && spaces > 0)
    }

    def movesToRoom(ix:Int, roomIndex:Int) = {

      // modify ix to match expanded corridor positions
      val iPos = ix match {
        case i if (i<=1) => i
        case i if (i > 1 && i < 5) => (ix * 2) - 1
        case i if (i >= 5 ) => i + 4
      }

      val roomPos = (roomIndex +1) * 2

      Math.abs(iPos - roomPos)

    }

    def copyRooms(roomIndex:Int) = {
      val rs = rooms.map(identity) // copy rooms
      val r = rs(roomIndex).map(identity) // copy individual rooms
      rs(roomIndex) = r
      (rs, r)
    }

    def minCost(): Int = {
      var min = Int.MaxValue
      availableMoves().foreach(m => {
        val location = locationsCache.getOrElseUpdate(m.result.toString, {
          if (m.result.isSorted()){
            0 // no moves as we are end point
          }
          else {
            m.result.minCost()
          }
        })
        if (location < Integer.MAX_VALUE) {
          min = Math.min(location + m.cost, min)
        }
      })
      min
    }

    def freeRoomPos(roomIndex:Int) = {
      val room = rooms(roomIndex)
      room.indices.findLast(ix => {
        room(ix) == ' '
      }).get
    }

    def availableMoves(): List[Move] = {
      val hallwayMoves = hallway.indices.flatMap(ix => {
        var result:List[Move] = List()
        if (hallway(ix) != ' ') {
          val pod = hallway(ix)
          val roomIndex = pod.toInt - charOffset
          val podRoom = rooms(roomIndex)
          if (canMoveIntoRoom(podRoom, pod) && !isHallwayBlocked(ix, roomIndex)) {
            //println("Move into room")
            val h = hallway.map(identity) // copy
            h(ix) = ' ' // clear hallway position
            val (rs, r) = copyRooms(roomIndex)
            val roomPos = freeRoomPos(roomIndex)
            r(roomPos) = pod // move into the room
            val l = Locations(h, rs) // create new locations

            val moves = movesToRoom(ix, roomIndex) + roomPos + 1

            result = Move(l, moves * podCost(pod)) :: result
          }
        }
        result
      })

      // pods in rooms into corridor positions
      val roomMoves = rooms.indices.flatMap(roomIndex => {
        val room = rooms(roomIndex)
        val roomChar = (charOffset + roomIndex).toChar
        val full = room.forall(_ == roomChar)

        // if we can move chars in, we can't move chars out
        if (canMoveIntoRoom(room, roomChar) || full) {
          List()
        }
        else {
          val roomPos = room.indices.find(i => room(i) != ' ').get
          val pod = room(roomPos)

          val availableCorridorPositions = hallway.indices.filter(ix => {
            hallway(ix) == ' ' && !isHallwayBlocked(ix, roomIndex)
          })

          availableCorridorPositions.map(cix => {
            val h = hallway.map(identity)
            val (rs, r) = copyRooms(roomIndex)
            h(cix) = pod // move to hallway
            r(roomPos) = ' ' // move from room
            val l = Locations(h, rs)
            val moves = movesToRoom(cix, roomIndex) + roomPos + 1
            Move(l, moves * podCost(pod))
          })
        }
      })
      (hallwayMoves ++ roomMoves).toList
    }

    def isSorted(): Boolean = rooms.indices.forall(ri => {
      val roomChar = (ri + charOffset).toChar
      rooms(ri).forall(_ == roomChar)
    })

    override def toString: String = {
      val roomString = rooms.map(r => "(" + r.mkString(",") + ")").mkString(",")
      s"Locations(hallway=[${hallway.mkString(",")}], rooms=[${roomString}]"
    }
  }
}
