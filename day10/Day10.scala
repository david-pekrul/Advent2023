package day10

import day10.Day10.Vector.{DOWN, LEFT, RIGHT, UP}
import helpers.Helpers

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day10/test1.txt")
    //    val input = Helpers.readFile("day10/test1_1.txt")
    //    val input = Helpers.readFile("day10/test2_1.txt")
    //    val input = Helpers.readFile("day10/test3.txt")
    //    val input = Helpers.readFile("day10/test4.txt")
    val input = Helpers.readFile("day10/test5.txt")
    //    val input = Helpers.readFile("day10/test6.txt")
    //    val input = Helpers.readFile("day10/day10.txt")
    val (coordToPipeMap, xDimension, yDimension) = parse(input)

    //    printMap(coordToPipeMap, xDimension, yDimension)

    val culledInput = cull(coordToPipeMap)

    //    printMap(culledInput, xDimension, yDimension)

    val burn2 = burnBothEnds2(culledInput)

    val (_, leftChain, rightChain, part1) = burn2
    println(s"Part 1: $part1")

    val fullChain = rightChain ++ leftChain.reverse.tail
    if (fullChain.head != fullChain.last) {
      ??? //sanity check the start node is the first and last
    }

    //    val part2 = countCapturedCoords(culledInput, coordsInLoop, xDimension, yDimension)
    //    println(s"Part 2: $part2")
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
    val halfWayPipe = finalState._1
    val coordsInLoop = finalState._3 + halfWayPipe.coord
    val lengthToHalf = finalState._4
    (halfWayPipe, coordsInLoop, lengthToHalf)
  }

  def burnBothEnds2(culledMap: Map[Coord, Pipe]) = {

    val startNode = culledMap.values.find(_.shape == 'S').get


    val startingNeighbors = startNode.getConnectionCoords().map(neighborCoord => culledMap.get(neighborCoord)).filter(_.isDefined).map(_.get).toSeq
    val startCondition = (startingNeighbors(0), startingNeighbors(1), Seq(startNode.coord, startingNeighbors(0).coord), Seq(startNode.coord, startingNeighbors(1).coord), 1) //"left path","right path","seen nodes"

    //create sequence of (Coord, Vector) so we know for each loop point, which way we are going
    val finalState = Iterator.iterate(startCondition)(currentState => {
      val nextLeftCoord = currentState._1.getConnectionCoords().filter(coord => !currentState._3.contains(coord)).head
      val nextRightCoord = currentState._2.getConnectionCoords().filter(coord => !currentState._3.contains(coord)).head
      val leftChain = currentState._3 :+ nextLeftCoord
      val rightChain = currentState._4 :+ nextRightCoord
      (culledMap(nextLeftCoord), culledMap(nextRightCoord), leftChain, rightChain, currentState._5 + 1)
    }).find(state => {
      state._1 == state._2
    }).get

    val (halfWayPipe, halfWayPipe2, leftChain, rightChain, lengthToHalf) = finalState
    (halfWayPipe, leftChain, rightChain, lengthToHalf)
  }

  def countCapturedCoords(input: Map[Coord, Pipe], coordsInLoop: Set[Coord], maxX: Int, maxY: Int): Int = {

    val filteredMap = input.filter(x => coordsInLoop.contains(x._1))

    def _inLoop(coord: Coord): Boolean = {
      //a point in the loop would have to have an odd number of crossings to get out

      //starting == (coord, pipes from loop crossed getting out)
      val (offTheEdge, runToEdge) = Iterator.iterate((coord, Set[Pipe]()))(currentState => {
        val nextCoord = currentState._1.up()
        val nextPipeOpt = filteredMap.get(nextCoord)
        val updatedSet = nextPipeOpt match {
          case Some(p) => currentState._2 + p
          case None => currentState._2
        }
        (nextCoord, updatedSet)
      })
        .find(n => n._1.offTheMap(maxX, maxY)).get

      val upSize = runToEdge.filter(_.shape != '|').size
      upSize % 2 == 1
    }

    (0 until maxX).map(x => {
      (0 until maxY).filter(y => {
        val c = Coord(x, y)
        !coordsInLoop.contains(c) && _inLoop(Coord(x, y))
      }).size
    }).sum
  }


  def paintNodes(input: Map[Coord, Pipe], coordsInLoop: Set[Coord], fullPath: Seq[Coord], maxX: Int, maxY: Int) = {
    val filteredMap = input.filter(x => coordsInLoop.contains(x._1))

    val coordsAndVectors = fullPath.sliding(2, 1).map { case Seq(point, next) => {
      point -> Vector(next.x - point.x, next.y - point.y)
    }
    }.toSeq

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

    def up() = Coord(x, y - 1)

    def down() = Coord(x, y + 1)

    def left() = Coord(x - 1, y)

    def right() = Coord(x + 1, y)

    def offTheMap(maxX: Int, maxY: Int): Boolean = {
      x < 0 || y < 0 || x > maxX || y > maxY
    }
  }

  object Vector extends Enumeration {
    val UP = Vector(0, -1)
    val RIGHT = Vector(1, 0)
    val DOWN = Vector(0, 1)
    val LEFT = Vector(-1, 0)
  }

  case class Vector(deltaX: Int, deltaY: Int) {
    def apply(coord: Coord): Coord = {
      Coord(coord.x + deltaX, coord.y + deltaY)
    }

    def getInOutDirection(): Vector = {
      this match {
        case UP => RIGHT
        case RIGHT => DOWN
        case DOWN => LEFT
        case LEFT => UP
      }
    }
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
