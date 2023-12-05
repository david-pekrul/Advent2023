package day5

import helpers.Helpers

object Day5 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("day5/test.txt")
    val input = Helpers.readFile("day5/day5.txt")
    val parsed = parse(input)
    println(parsed)
  }

  def parse(input: Seq[String]) = {
    val itr = input.iterator
    val seedsLine = itr.next().replaceAll(".*:","").trim
    itr.next()//blank line after seeds
    val seedToSoilLine = getNextLines(itr)
    val soilToFertLine = getNextLines(itr)
    val fertToWaterLine = getNextLines(itr)
    val waterToLight = getNextLines(itr)
    val lightToTemp = getNextLines(itr)
    val tempToHumid = getNextLines(itr)
    val humidToLocation = getNextLines(itr)
    (seedsLine,soilToFertLine)
  }

  def getNextLines(itr: Iterator[String]): Seq[String] = {
    itr.takeWhile(s => !s.trim.isEmpty).toSeq.filter(!_.contains(":"))
  }
}
