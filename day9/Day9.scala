package day9

import helpers.Helpers

object Day9 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day9/test.txt")
    val input = Helpers.readFile("day9/day9.txt")

    val steps = input.map(parse)

    val part1 = steps.map(x => (x -> x.findNextValue())).map(_._2).sum
    println(s"Part1: $part1")

    val part2 = steps.map(x => (x -> x.findPrevValue())).map(_._2).sum
    println(s"Part2: $part2")

  }

  def parse(line: String): Steps = {
    Steps(line.split(" ").map(_.toLong).toSeq)
  }

  case class Steps(steps: Seq[Long]) {
    def findNextValue(): Long = {
      if (steps.forall(_ == 0)) {
        return 0
      }
      steps.last + reduce().findNextValue()
    }

    def findPrevValue(): Long = {
      if (steps.forall(_ == 0)) {
        return 0
      }
      steps.head - reduce().findPrevValue()
    }

    private def reduce(): Steps = {
      val allPairs = steps.sliding(2, 1).toSeq

      val x = allPairs.map(pair => pair.last - pair.head)
      Steps(x)
    }
  }
}
