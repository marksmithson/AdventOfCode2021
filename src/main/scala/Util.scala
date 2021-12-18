package me.smithson

object Util {
  def PrintGrid(grid: Array[Array[Int]]) = {
    grid.foreach(row => println(row.map(_.toString.padTo(2," ").mkString).mkString(".")))
  }
}
