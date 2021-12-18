package me.smithson

import scala.io.Source

object DataLoader {
  def Load = (name: String) => {
    Source.fromFile(s"./data/${name}.txt").getLines()
  }
  def LoadList = (name: String) => {
    Source.fromFile(s"./data/${name}.txt").getLines().toList
  }
}
