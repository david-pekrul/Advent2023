package day2

import helpers.Helpers

object Day2 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day2/test.txt")
    val input = Helpers.readFile("day2/day2.txt")

    val restrictions: Map[String, Int] = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )

    //    input.foreach(parseLine)

    val validGames = input.map(parseLine).filter(game => {
      val (id, phases) = game
      phases.forall(phase => {
        phase.forall { case (phaseColor, phaseAmount) => {
          phaseAmount <= restrictions.get(phaseColor).get
        }
        }
      })
    })

    val part1 = validGames.map(_._1).sum

    println(s"Part 1: $part1")

  }

  def parseLine(line: String) = {
    val gameIdRegex = """^Game (\d+):.*$""".r //regex has to match the full string
    val phaseRegex = """^(\d+) (\w+)""".r
    val groupings = line.replaceAll("^.*:", "").split(";").toSeq.map(_.trim)

    val gameIdRegex(gameId) = line

    val parsed = groupings.map(phase => {
      phase.split(",").map(_.trim).map(x => {
        val phaseRegex(amount, color) = x
        (color -> Integer.parseInt(amount))
      }).toMap
    })

    val game = (Integer.parseInt(gameId), parsed)
    //    println(game)
    game
  }
}
