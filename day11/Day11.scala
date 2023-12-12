package day11

import helpers.Helpers

object Day11 {
  def main(args: Array[String]): Unit = {
//        val input = Helpers.readFile("day11/test.txt")
    val input = Helpers.readFile("day11/day11.txt")
    val galaxies = parse(input)

    val expansion = findExpandedRowsAndColumns(galaxies)
    val indexGalaxies = galaxies.toSeq.zipWithIndex

    val galaxyPairs = indexGalaxies.foldLeft(Map[Coord, Seq[Coord]]())((acc, nextGalaxy) => {
      val galaxiesToPair = indexGalaxies.takeWhile(g => g != nextGalaxy).map(_._1)
      acc + (nextGalaxy._1 -> galaxiesToPair)
    }).toSeq.flatMap(kv => kv._2.map(x => (kv._1, x)))

    val part1 = galaxyPairs.map(galaxyPair => galaxyPair._1.spaceBetweenLARGE(galaxyPair._2, expansion)).sum
    println(s"Part 1: $part1")

    val scaleFactor = Math.pow(10,6).longValue

    val part2 = galaxyPairs.map(galaxyPair => galaxyPair._1.spaceBetweenLARGE(galaxyPair._2, expansion, scaleFactor)).sum
    println(s"Part 2: $part2")
  }

  def findExpandedRowsAndColumns(galaxies: Set[Coord]): Expansion = {
    val rowsWithGalaxies = galaxies.map(g => g.y)
    val columnsWithGalaxies = galaxies.map(g => g.x)

    val maxCol = columnsWithGalaxies.max
    val maxRow = rowsWithGalaxies.max

    val expandedCols = (0 until maxCol).toSet.removedAll(columnsWithGalaxies)
    val expandedRows = (0 until maxRow).toSet.removedAll(rowsWithGalaxies)

    Expansion(expandedRows, expandedCols)
  }

  def parse(input: Seq[String]): Set[Coord] = {
    input.zipWithIndex.flatMap(rowWithIndex => {
      rowWithIndex._1.toCharArray.toSeq.zipWithIndex.filter(_._1 == '#').map(charWithColIndex => {
        Coord(charWithColIndex._2, rowWithIndex._2)
      })
    }).toSet
  }

  case class Expansion(rows: Set[Int], cols: Set[Int]) {}

  case class Coord(x: Int, y: Int) {
    def spaceBetweenLARGE(other: Coord, exp: Expansion, scaleFactor: Long = 2 ): Long = {

      val leftX = Math.min(this.x, other.x)
      val rightX = Math.max(this.x, other.x)
      val topY = Math.min(this.y, other.y)
      val bottomY = Math.max(this.y, other.y)

      val expansionX = exp.cols.filter(e => leftX < e && e < rightX)
      val expansionY = exp.rows.filter(e => topY < e && e < bottomY)

      val deltaX = (rightX - leftX - expansionX.size) + (expansionX.size * scaleFactor)
      val deltaY = (bottomY - topY - expansionY.size) + (expansionY.size * scaleFactor)

      deltaX + deltaY
    }
  }
}
