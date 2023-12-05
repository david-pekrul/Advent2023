package day3

import helpers.Helpers

object Day3 {
  def main(args: Array[String]): Unit = {

    //    val input = Helpers.readFile("day3/test.txt")
    //    val input = Helpers.readFile("day3/test2.txt")
    val input = Helpers.readFile("day3/day3.txt")

    val parsed = input.zipWithIndex.map { case (line, index) => {
      parse(line, index)
    }
    }

    val builtData = buildMap(parsed)

    val possibleNeighbors = builtData._1
      .flatMap(_._1.getNeighbors())
      .map(symbolNeighbor => builtData._2.get(symbolNeighbor))
    val thing = possibleNeighbors
      .filter(_.isDefined)
    val numbersNextToSymbols = thing
      .map(_.get)
      .distinct
      .map(id => builtData._3.get(id).get)
      .toSeq.sorted

    val part1 = numbersNextToSymbols.sum

    println(s"Part 1: $part1")

    val part2 = findGears(builtData)
    println(s"Part 2: $part2")

  }

  def parse(line: String, row: Int): Row = {

    val numberFinder = """(\d+)""".r
    val expectedNumbers = numberFinder.findAllIn(line).size

    val x = line.toCharArray.zipWithIndex.toSeq.foldLeft((("", Set[Coord]()), Row(Set(), Seq())))((acc, nextChar) => {
      if (nextChar._1 == '.') {
        if (acc._1._1.isEmpty) {
          acc //no new digit
        } else {
          //found the end of a number
          val fullNumber = acc._1._1
          val updatedRow = Row(acc._2.numbers + (fullNumber -> acc._1._2), acc._2.symbols)
          (("", Set[Coord]()), updatedRow)
        }
      } else if (nextChar._1.isDigit) {
        val appendedNumber = acc._1._1 + nextChar._1
        val appendedCoords = acc._1._2 + Coord(nextChar._2, row)
        ((appendedNumber, appendedCoords), acc._2)
      } else {
        val updatedRow1 = if (acc._1._1.isEmpty) {
          acc //no new digit
        } else {
          //found the end of a number
          val fullNumber = acc._1._1
          val updatedRow = Row(acc._2.numbers + (fullNumber -> acc._1._2), acc._2.symbols)
          (("", Set[Coord]()), updatedRow)
        }
        val symbolCoord = Coord(nextChar._2, row)
        val updatedRow = Row(updatedRow1._2.numbers, updatedRow1._2.symbols :+ (symbolCoord, nextChar._1))
        (("", Set[Coord]()), updatedRow)
      }
    })


    val finalRow = if (!x._1._1.isEmpty) {
      //the line ended with a number
      val fullNumber = x._1._1
      val updatedRow = Row(x._2.numbers + (fullNumber -> x._1._2), x._2.symbols)
      updatedRow
    } else {
      x._2
    }

    if (finalRow.numbers.size != expectedNumbers) {
      ???
    }

    finalRow


  }

  def buildMap(rows: Seq[Row]) = {
    //will it be a problem that I don't have an ID for each number?
    //is breaking a number up by coord a good thing?
    val allSymbols = rows.flatMap(_.symbols)

    val allNumbersMap = rows.flatMap(row => row.numbers.map(kv => (kv._1.toLong -> kv._2))).toMap

    val x = rows.flatMap(row => row.numbers).zipWithIndex.map(a => (a._1._1, a._2, a._1._2)) //number, index, coords

    val idToNumber = x.map(a => (a._2 -> a._1.toInt)).toMap
    val coordToId = x.flatMap(a => (a._3).map(coord => coord -> a._2)).toMap

    val coordToNumber = allNumbersMap.flatMap(kv => kv._2.map(c => c -> kv._1))
    (allSymbols, coordToId, idToNumber)
  }


  def findGears(builtData: (Seq[(Coord, Char)], Map[Coord, Int], Map[Int, Int])): Int = {
    val gears = builtData._1
      .filter(x => x._2 == '*')
    val gearNeighborsAll = gears
      .map(gear => gear._1
        .getNeighbors()
        .map(gearNeighbor => builtData._2.get(gearNeighbor)).filter(_.isDefined).map(_.get).distinct
      )
    val filteredGearNeighbors = gearNeighborsAll
      .filter(gearSet => gearSet.size == 2)
    val calc = filteredGearNeighbors
      .map(gearSet => gearSet.map(id => builtData._3(id)).reduce((a, b) => a * b))
      .sum
    calc
  }
}

case class Coord(x: Int, y: Int) {
  def getNeighbors(): Seq[Coord] = {
    Seq(
      Coord(x, y + 1), //down
      Coord(x, y - 1), //up
      Coord(x + 1, y), //right
      Coord(x - 1, y), //left
      Coord(x + 1, y + 1), //DR
      Coord(x + 1, y - 1), //DL
      Coord(x - 1, y + 1),
      Coord(x - 1, y - 1)
    )
  }
}


//numbers can't be a Map.  There might be duplicated numbers in the same row.
case class Row(numbers: Set[(String, Set[Coord])], symbols: Seq[(Coord, Char)]) {}