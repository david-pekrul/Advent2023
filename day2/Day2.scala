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

    val parsedGames = input.map(parseLine)

    val validGames = parsedGames.filter(game => {
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

    val part2 = parsedGames.map { case (id, phases) => {
      val maxPerColor = phases.flatten
        .groupBy(_._1)
        .map(colorGroup => colorGroup._1 -> colorGroup._2.map(_._2).max)
      val mult = maxPerColor.values.reduce((a, b) => a * b)
      (id, mult)
    }
    }
      .map(_._2).sum


    println(s"Part 2: $part2")

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
