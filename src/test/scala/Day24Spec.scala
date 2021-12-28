package me.smithson

import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec

class Day24Spec extends AnyFlatSpec with BeforeAndAfterEach {
  "forward" should "reverse" in {
    val d = 26
    val a1 = -1
    val a2 = 5
    val i = -5
    val r = Day24.step(d,a1,a2,i,1)

    println(r)

    val rev = Day24.zRegisterForResult(d,a1,a2,r,1)

    assert(rev.get.contains(i))
  }

  "forwards" should "reverse" in {
    val d = 26
    val a1 = -1
    val a2 = 5
    val i = 0
    val r = Day24.step(d,a1,a2,i,1)

    println(r)

    val rev = Day24.zRegisterForResult(d,a1,a2,r,1)

    assert(rev.get.contains(i))
  }
}
