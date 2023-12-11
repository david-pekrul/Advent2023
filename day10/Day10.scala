package day10

import helpers.Helpers

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day10/test1.txt")
    //    val input = Helpers.readFile("day10/test1_1.txt")
    //    val input = Helpers.readFile("day10/test2_1.txt")
    val input = Helpers.readFile("day10/day10.txt")
    val (coordToPipeMap, xDimension, yDimension) = parse(input)

    //    printMap(coordToPipeMap, xDimension, yDimension)

    val culledInput = cull(coordToPipeMap)

    //    printMap(culledInput, xDimension, yDimension)

    val futhestPipeState = burnBothEnds(culledInput)

    val part1 = futhestPipeState._4
    println(s"Part 1: $part1")
  }

  def parse(input: Seq[String]): (Map[Coord, Pipe], Int, Int) = {
    val map = input.zipWithIndex.flatMap(lineWithRowIndex => {
      val row = lineWithRowIndex._2
      lineWithRowIndex._1.toCharArray.toSeq.zipWithIndex.map(charWithColIndex => {
        val char = charWithColIndex._1
        val col = charWithColIndex._2
        Pipe(char, Coord(col, row))
      })
    }).map(pipe => (pipe.coord -> pipe)).toMap

    val x = input(0).length
    val y = input.size
    (map, x, y)
  }

  def burnBothEnds(culledMap: Map[Coord, Pipe]) = {

    val startNode = culledMap.values.find(_.shape == 'S').get


    val startingNeighbors = startNode.getConnectionCoords().map(neighborCoord => culledMap.get(neighborCoord)).filter(_.isDefined).map(_.get).toSeq
    val startCondition = (startingNeighbors(0), startingNeighbors(1), Set(startNode.coord, startingNeighbors(0).coord, startingNeighbors(1).coord), 1) //"left path","right path","seen nodes"

    val finalState = Iterator.iterate(startCondition)(currentState => {
      val nextLeftCoord = currentState._1.getConnectionCoords().filter(coord => !currentState._3.contains(coord)).head
      val nextRightCoord = currentState._2.getConnectionCoords().filter(coord => !currentState._3.contains(coord)).head

      (culledMap(nextLeftCoord), culledMap(nextRightCoord), currentState._3 ++ Set(nextLeftCoord, nextRightCoord), currentState._4 + 1)
    }).find(state => {
      state._1 == state._2
    }).get

//    println(finalState._1, finalState._4)

    finalState

  }


  @tailrec
  def cull(input: Map[Coord, Pipe]): Map[Coord, Pipe] = {
    //remove the entries that don't form connections with its 2 neighbors
    //Note: Not instant, but fast enough :-)
    val culledMap = input.filter(currentCoordPipe => {
      val foundNeighbors = currentCoordPipe._2
        .getConnectionCoords()
        .map(connectionCoord => input.get(connectionCoord))
        .filter(_.isDefined)
        .map(_.get)

      foundNeighbors.filter(neighbor => neighbor.getConnectionCoords().contains(currentCoordPipe._1)).size == 2
    })

    if (culledMap.size != input.size) {
      cull(culledMap)
    } else {
      culledMap
    }
  }

  case class Pipe(shape: Char, coord: Coord) {
    def getConnectionCoords(): Set[Coord] = {
      shape match {
        case '|' => Set(coord.up, coord.down)
        case '-' => Set(coord.left, coord.right)
        case 'L' => Set(coord.up, coord.right)
        case 'J' => Set(coord.up, coord.left)
        case 'F' => Set(coord.right, coord.down)
        case '7' => Set(coord.left, coord.down)
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

  def printMap(map: Map[Coord, Pipe], xMax: Int, yMax: Int): Unit = {
    (0 until yMax).foreach(y => {
      (0 until xMax).foreach(x => {
        print(map.get(Coord(x, y)).map(_.shape).getOrElse('.'))
      })
      println
    })
    println("---------------------------")
  }
}
