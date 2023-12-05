package day4

import helpers.Helpers

object Day4 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("day4/test.txt")
    val input = Helpers.readFile("day4/day4.txt")

    val part1 = input.map(Game.parse).map(_.score).sum
    println(s"Part 1: $part1")
  }

  case class Game(winningNumbers: Seq[Int], myNumbers: Seq[Int]) {
    def score(): Int = {
      myNumbers.filter(winningNumbers.contains(_)).size match {
        case 0 => 0
        case i: Int => Math.pow(2,i-1).intValue()
      }
    }
  }

  object Game {
    //game id: (winning numbers) | (my numbers)
    val gameRegex = """^.*:([\d\s]+)\|([\d\s]+)$""".r

    def parse(line: String): Game = {
      val gameRegex(winningString,myString) = line
      Game(getNumbers(winningString),getNumbers(myString))
    }

    private def getNumbers(input: String): Seq[Int] = {
      input.trim.split("""\s+""").map(_.toInt)
    }
  }
}
