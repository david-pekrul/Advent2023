package day11

import helpers.Helpers

object Day11 {
  def main(args: Array[String]): Unit = {
    val input = Helpers.readFile("day11/test.txt")

    val galaxies = parse(input)
    println(galaxies)

    val indexGalaxies = galaxies.toSeq.zipWithIndex

    val galaxyPairs = indexGalaxies.foldLeft(Map[Coord,Seq[Coord]]())((acc,nextGalaxy) => {
      val galaxiesToPair = indexGalaxies.takeWhile(g => g != nextGalaxy).map(_._1)
      acc + (nextGalaxy._1 -> galaxiesToPair)
    })

    val y = galaxyPairs.toSeq.flatMap(kv => kv._2.map(x => (kv._1,x)))

  }

  def findExpandedRowsAndColumns(galaxies: Set[Coord]): (Set[Int],Set[Int]) = {
    val rowsWithGalaxies = galaxies.map(g => g.y)
    val columnsWithGalaxies = galaxies.map(g => g.x)


    ???
  }

  def parse(input: Seq[String]): Set[Coord] = {
    input.zipWithIndex.flatMap(rowWithIndex => {
      rowWithIndex._1.toCharArray.toSeq.zipWithIndex.filter(_._1=='#').map(charWithColIndex => {
        Coord(charWithColIndex._2,rowWithIndex._2)
      })
    }).toSet
  }

  case class Coord(x: Int, y: Int) {

    def up() = Coord(x, y - 1)

    def down() = Coord(x, y + 1)

    def left() = Coord(x - 1, y)

    def right() = Coord(x + 1, y)

    def offTheMap(maxX: Int, maxY: Int): Boolean = {
      x < 0 || y < 0 || x > maxX || y > maxY
    }

    def spaceBetween(other: Coord): Int = {

      -1
    }
  }
}
