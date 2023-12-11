package day10

import helpers.Helpers

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = Helpers.readFile("day10/test1.txt")
    val coordToPipeMap = parse(input)
    println(coordToPipeMap)
  }

  def parse(input: Seq[String]) = {
    input.zipWithIndex.flatMap(lineWithRowIndex => {
      val row = lineWithRowIndex._2
      lineWithRowIndex._1.toCharArray.toSeq.zipWithIndex.map(charWithColIndex => {
        val char = charWithColIndex._1
        val col = charWithColIndex._2
        Pipe(char, Coord(col, row))
      })
    }).map(pipe => (pipe.coord -> Pipe)).toMap
  }

  case class Pipe(shape: Char, coord: Coord) {
    def getConnectionCoords(): Set[Coord] = {
      shape match {
        case '|' => Set(coord.up, coord.down)
        case '-' => Set(coord.left, coord.right)
        case 'L' => Set(coord.up, coord.right)
        case 'J' => Set(coord.up, coord.left)
        case 'F' => Set(coord.right, coord.down)
        case '.' => Set()
        case 'S' => Set(coord.up, coord.down, coord.left, coord.right) //??
      }
    }
  }

  case class Coord(x: Int, y: Int) {
    //    def getNeighbors(): Seq[Coord] = {
    //      Seq(
    //        Coord(x, y + 1), //down
    //        Coord(x, y - 1), //up
    //        Coord(x + 1, y), //right
    //        Coord(x - 1, y), //left
    //        Coord(x + 1, y + 1), //DR
    //        Coord(x + 1, y - 1), //DL
    //        Coord(x - 1, y + 1),
    //        Coord(x - 1, y - 1)
    //      )
    //    }

    def up() = Coord(x, y - 1)

    def down() = Coord(x, y + 1)

    def left() = Coord(x - 1, y)

    def right() = Coord(x + 1, y)
  }
}
