package day6

import helpers.Helpers

object Day6 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day6/test.txt")
    val input = Helpers.readFile("day6/day6.txt")
    val races = Race.parse(input(0), input(1))
    val part1 = races.map(_.countWinConditions()).reduce((a, b) => a * b)
    println(s"Part 1: $part1")

    val part2 = Race.parsePart2(input(0), input(1)).countWinConditions()
    println(s"Part 2: $part2")
  }

  case class Race(time: Long, distance: Long) {
    def countWinConditions(): Int = {
      val discriminant = time * time - 4 * distance
      if (discriminant < 0) {
        return 0
      }

      val plus = (time + Math.sqrt(discriminant)) / 2.0
      val minus = (time - Math.sqrt(discriminant)) / 2.0

      //how many integers are between these two values?
      val range = (Math.ceil(minus).intValue to Math.floor(plus).intValue)
      val count = range.size - Seq(plus, minus).filter(_.isWhole).size
      count
    }
  }

  object Race {
    def parse(timeLine: String, distanceLine: String): Seq[Race] = {
      val times = getNumbers(timeLine)
      val distances = getNumbers(distanceLine)
      times.zip(distances).map(x => Race(x._1, x._2))
    }

    def parsePart2(timeLine: String, distanceLine: String): Race = {
      val time = getPart2Number(timeLine)
      val distance = getPart2Number(distanceLine)
      Race(time, distance)
    }


    def getNumbers(line: String): Seq[Int] = {
      line.replaceAll(""".*:""", "").trim.split("""\s+""").map(_.toInt).toSeq
    }

    val digitRegex = """(\d+)""".r

    def getPart2Number(line: String): Long = {
      digitRegex.findAllMatchIn(line).toSeq
        .map(_.group(0))
        .mkString("").toLong
    }
  }
}
