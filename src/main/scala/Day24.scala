package me.smithson

object Day24 extends Day {
  val registerOffset = 'w'.toInt

  def isRegister(str: String) = {
    val ch = str.charAt(0)
    val regIx = ch.toInt - registerOffset
    (regIx >= 0 && regIx <= 3)
  }

  def step (d:Int, a1:Int, a2:Int, z:Int, w:Int) = {
    if ((z % 26 + a1) == w) {
      z/d
    }
    else {
      ((z/d) * 26) + w + a2
    }
  }

  def zRegisterForResult(d:Int, a1:Int, a2:Int, r:Int, w:Int) = {
    var result:List[Int] = List()
    val u = r-w-a2
    val q = w-a1
    if (u % 26 == 0) {
      // z = d*(r-w-a2)/26
      val c = (u/26 * d)
      result = c :: result
    }
    if (q >=0 && q < 26) { // z % 26 == w-a1 - values which make this true
      val c = r * d
      result = (q + c) :: result // add q - this is the bit which gets "lost" in integer division
    }

    if (result.isEmpty){
      None
    }
    else {
      Some(result)
    }
  }

  /**
   * To get to work in a reasonable time frame, we need to know more about the program we are running.
   *
   * Turns out there are 14 steps which process the input, where each step can be represented by the
   * step function above where d, a1, and a2 are hard coded into the program.
   *
   * d = div z 1 (instruction index 4)
   * a1 = add x 14 (ix 5)
   * a2 = add y 2 (ix 15)
   *
   * each step uses the z register from the previous step
   *
   * knowing this we can work backwards from the final result of 0 to work out the corresponding
   * values of z we need from the previous step for each possible input 1-9
   *
   * this is the zRegisterForResult function below. This uses the following information from an examination of the program
   * - d can be 1 or 26
   * - when d is 26, a1 is < -1
   * - w can be 1-9
   *
   * @param data
   * @return
   */
  override def Part1(data: Iterator[String]): String = {
    val program = parseData(data)
    validNumbers(program).max.toString
    //94992994195998
  }


  override def Part2(data: Iterator[String]): String = {
    val program = parseData(data)
    validNumbers(program).min.toString
    //21191861151161
  }

  def validNumbers(program:List[Array[String]]) = {
    val inputIndices = program.indices.filter(ix => {
      program(ix)(0) == "inp"
    }).reverse

    val blockLength = inputIndices(0) - inputIndices(1)

    val blocks = inputIndices.map(ix => {
      val block = program.slice(ix, ix + blockLength)
      val d = Integer.parseInt(block(4)(2))
      val a1 = Integer.parseInt(block(5)(2))
      val a2 = Integer.parseInt(block(15)(2))
      (d,a1,a2)
    })

    def inputsForResult(block:(Int,Int,Int), r:Int) = {
      val (d,a1,a2) = block
      (1 to 9).map(w => {
        val result = zRegisterForResult(d, a1, a2, r, w)
        result match {
          case Some(c) => Some(Input(w,c))
          case None => None
        }
      }).filter(_.isDefined).map({case Some(r) => r}).toList
    }

    case class Input(w:Int, zs:List[Int]) {
      var inputStacks: List[Input] = List()
      var parent: Option[Input] = None

      def updateParents() = {
        inputStacks.foreach(in => in.parent = Some(this))
      }

      def number():String = {
        parent match {
          case Some(p) => s"${w}${p.number()}"
          case None => w.toString
        }
      }
    }

    def processResults(r: List[Input], blkIx:Int):Unit = {
      r.foreach(input => {
        val res = input.zs.flatMap( z=> {
          inputsForResult(blocks(blkIx), z)
        })
        input.inputStacks = res
        input.updateParents()
        if (blkIx < 13) {
          processResults(res, blkIx + 1)
        }
      })
    }

    val results:List[Input] = inputsForResult(blocks(0), 0)
    processResults(results, 1)

    // find the largest number for block 14
    var digits = results
    (0 to 12).foreach(_ => {
      digits = digits.flatMap(d=>d.inputStacks)
    })

    // valid first digits starts with a 0 (empty register)
    val m0 = digits.filter(i => i.zs.contains(0))
    m0.map(i => i.number().toLong)
  }

  /**
   * Original Attempt
   *
   * Runs too slowly....
   *
   * Tried some optimisations
   *  - avoiding string parsing
   *  - caching program state just before new inputs
   *
   *  Gonna need a different approach - should have realised this earlier
   */
  def Part1_Attempt1(data: Iterator[String]): String = {
    val program = parseData(data)

    def registerOrStatic(str:String) = {
      str match {
        case s if isRegister(s) => {
          val reg = str.charAt(0).toInt - registerOffset
          (regs: Int => Long) => {
            regs(reg)
          }
        }
        case i => {
          val r = Integer.parseInt(i)
          (_: Int => Long) => r.toLong
        }
      }
    }

    val instructions = program.map(line => {
      (line(0), line(1).charAt(0).toInt - registerOffset, if (line.length > 2) registerOrStatic(line(2)) else (_:Int=>Long) => 0L)
    })

    val current = 99999999999999L
    val input:Array[Int] = current.toString.toCharArray.map(c=>{Integer.parseInt(c.toString)})

    val inputIndices = instructions.indices.filter(ix => {
      instructions(ix)._1 == "inp"
    })

    def updateCache(ix:Int, registers:Array[Long]) = {
      val last = if (ix == 13) instructions.length else inputIndices(ix + 1)
      val updateInput = input.slice(ix, ix + 1)
      val updatedRegisters = runProgram(instructions.slice(inputIndices(ix), last), registers, updateInput)
      updatedRegisters.map(identity)
    }

    val registers:Array[Long] = Array.fill(4)(0L)

    // run first batch
    val caches = input.indices.map { ix => {
        updateCache(ix, registers)
      }
    }.toArray

    //caches.foreach(c => println(c.mkString(",")))

    def decrement(col:Int):Unit = {
      caches(col) = Array() // clear cache for this column
      input(col) -= 1
      if (input(col) == 0) {
        input(col) = 9
        decrement(col - 1)
      }
      else {
        val updateRegisters = if (col == 0) Array.fill(4)(0L) else caches(col - 1)
        if (updateRegisters.isEmpty){
          println("PANIC")
        }
        (col until input.length).foreach(ix => {
          caches(ix) = updateCache(ix, updateRegisters)
        })
      }
    }
    var run = 0
    while (caches(13)(3) != 0) {
      run += 1
      if (run %500_000 == 0) {
        println(s"z=${caches(13)(3)} for ${input.mkString("")}")
      }
      decrement(13)
    }

    input.mkString("")
  }

  var cache:scala.collection.mutable.HashMap[String, Array[Long]] = scala.collection.mutable.HashMap()

  def runProgram(program:List[(String, Int, (Int => Long) => Long)], registers:Array[Long], input:Array[Int]):Array[Long] = {

    var ix = - 1
    def nextInput():Int = {
      ix += 1
      input(ix)
    }

    program.foreach {
      case ("inp", reg, _) => {
        val next = nextInput()
        registers(reg) = next
      }
      case ("add", l, r) => {
        registers(l) += r(registers)
      }
      case ("mul", l, r) => {
        registers(l) *= r(registers)
      }
      case ("div", l, r) => {
        registers(l) /= r(registers)
      }
      case ("mod", l, r) => {
        registers(l) %= r(registers)
      }
      case ("eql", l, r) => {
        registers(l) = if (registers(l) == r(registers)) 1 else 0
      }
    }

    registers
  }


  def parseData(data: Iterator[String]) = {
    data.map(_.split(" ")).toList
  }
}
