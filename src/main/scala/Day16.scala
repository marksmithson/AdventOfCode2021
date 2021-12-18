package me.smithson


object Day16 extends Day {
  def Part1(data: Iterator[String]) = {
    val bits = parseData(data)
    val packet = parsePacket(bits.iterator)
    packet.versionSum.toString
  }
  def Part2(data: Iterator[String]) = {
    val bits = parseData(data)
    val packet = parsePacket(bits.iterator)
    packet.calculate.toString
  }
  def parseData(data: Iterator[String]) = {
    val hex = data.next().toCharArray
    hex.flatMap(hexChar => {
      val i = Integer.parseInt(hexChar.toString, 16)
      i.toBinaryString.reverse.padTo(4,'0').reverse.toCharArray
    })
  }

  def parsePacket(data: Iterator[Char]): Packet = {
    // first 3 bits are the version of the packet
    val version = Integer.parseInt(data.take(3).mkString,2)

    val packetType = Integer.parseInt(data.take(3).mkString,2)
    // assume type 4 - this means that we read 5 bits until the leading char is 0 for a group

    if (packetType == 4) {
      val literalValue = parseLiteralValue(data)
      LiteralPacket(version, literalValue)
    }
    else {
      val lengthType = data.next()

      val subPackets:List[Packet] = lengthType match {
        case '0' => {
          parsePacketBytes(data)
        }
        case '1' => {
          parseSubPackets(data)
        }
      }
      OperatorPacket(version, packetType, subPackets)
    }
  }

  def parseSubPackets(data: Iterator[Char]): List[Packet] = {
    val packets = Integer.parseInt(data.take(11).mkString, 2)
    (1 to packets).map(_=>{
      parsePacket(data)
    }).toList
  }

  def parsePacketBytes(data: Iterator[Char]): List[Packet] = {
    val bytes = Integer.parseInt(data.take(15).mkString, 2)
    val dataToParse = data.take(bytes).mkString.toCharArray.iterator // hacky way to get an independent iterator
    var result: List[Packet] = List()
    while(dataToParse.hasNext) {
      result = parsePacket(dataToParse) :: result
    }
    result.reverse
  }

  def parseLiteralValue(data: Iterator[Char]) = {
    val literal = new StringBuilder()
    var moreGroups = true
    while (moreGroups) {
      val group = data.take(5)
      if (group.next() == '0') moreGroups = false
      literal.appendAll(group)
    }
    java.lang.Long.parseLong(literal.toString(), 2)
  }
}

trait Packet {
  def version: Int
  def versionSum: Int
  def packetType: Int
  def calculate: Long
}

case class LiteralPacket(packetVersion: Int, value:Long ) extends Packet {
  override def version: Int = packetVersion
  override def versionSum: Int = packetVersion
  override def packetType: Int = 4
  override def calculate: Long = value
}

case class OperatorPacket(packetVersion: Int, packType:Int, subPackets:List[Packet]) extends Packet {
  override def version: Int = packetVersion
  override def versionSum: Int = {
    subPackets.map(p => p.versionSum).sum + version
  }
  override def packetType: Int = 4
  override def calculate: Long = {
    val subPacketValues = subPackets.map(p => p.calculate)
    packType match {
      case 0 => subPacketValues.sum
      case 1 => subPacketValues.product
      case 2 => subPacketValues.min
      case 3 => subPacketValues.max
      case 5 => if (subPacketValues(0) > subPacketValues(1)) 1 else 0
      case 6 => if (subPacketValues(0) < subPacketValues(1)) 1 else 0
      case 7 => if (subPacketValues(0) == subPacketValues(1)) 1 else 0
    }
  }
}
