package day4

import helpers.Helpers

object Day4 {
  def main(args: Array[String]): Unit = {
    //        val input = Helpers.readFile("day4/test.txt")
    val input = Helpers.readFile("day4/day4.txt")

    val parsedGames = input.map(Game.parse)
    val part1 = parsedGames.map(_.score).sum
    println(s"Part 1: $part1")

    val part2 = scorePart2(parsedGames)
    println(s"Part 2: $part2")
  }

  def scorePart2(games: Seq[Game]): Int = {

    val initialMap: Map[Int, Int] = games.map(game => (game.id -> 1)).toMap

    val finalValues = games.foldLeft(initialMap)((acc, currentGame) => {
      val wins = currentGame.countWins()
      val scale = acc(currentGame.id)
      val updatedMap = (currentGame.id + 1 to currentGame.id + wins).foldLeft(acc)((currentMap, nextId) => {
        currentMap.updatedWith(nextId) {
          case Some(factor) => Some(factor + scale)
          case None => None
        }
      })

      updatedMap
    })

    val sortedValues = finalValues.toSeq.sortBy(a => a._1)

    finalValues.values.sum
  }

  case class Game(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    def score(): Int = {
      countWins() match {
        case 0 => 0
        case i: Int => Math.pow(2, i - 1).intValue()
      }
    }

    def countWins(): Int = {
      myNumbers.filter(winningNumbers.contains(_)).size
    }
  }

  object Game {
    //game id: (winning numbers) | (my numbers)
    val gameRegex = """^Card\s+(\d+):([\d\s]+)\|([\d\s]+)$""".r

    def parse(line: String): Game = {
      val gameRegex(id, winningString, myString) = line
      val winningnumbers = getNumbers(winningString)
      if (winningnumbers.size != winningnumbers.toSet.size) {
        ??? // verify winning numbers are unique
      }
      val myNumbers = getNumbers(myString)
      if (myNumbers.size != myNumbers.toSet.size) {
        ??? //verify my numbers are unique
      }
      Game(id.toInt, winningnumbers.toSet, myNumbers.toSet)
    }

    private def getNumbers(input: String): Seq[Int] = {
      input.trim.split("""\s+""").map(_.toInt)
    }
  }
}
