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
    //    val input = Helpers.readFile("day10/test5.txt")
    //    val input = Helpers.readFile("day10/test6.txt")
    val input = Helpers.readFile("day10/day10.txt")
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


    val x = paintNodes(coordToPipeMap, fullChain.toSet, fullChain, xDimension, yDimension)
    val part2 = x.size
    println(s"Part 2: $part2")
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

  def burnBothEnds2(culledMap: Map[Coord, Pipe]) = {

    val startNode = culledMap.values.find(_.shape == 'S').get


    val startingNeighbors = startNode.getConnectionCoords().map(neighborCoord => culledMap.get(neighborCoord)).filter(_.isDefined).map(_.get).toSeq
    val startCondition = (startingNeighbors(0), startingNeighbors(1), Seq(startNode.coord, startingNeighbors(0).coord), Seq(startNode.coord, startingNeighbors(1).coord), 1) //"left path","right path","seen nodes"

    //create sequence of (Coord, Vector) so we know for each loop point, which way we are going
    val finalState = Iterator.iterate(startCondition)(currentState => {
      val nextLeftCoord = currentState._1.getConnectionCoords().filter(coord => !currentState._3.contains(coord)).head
      val nextRightCoord = currentState._2.getConnectionCoords().filter(coord => !currentState._4.contains(coord)).head
      val leftChain = currentState._3 :+ nextLeftCoord
      val rightChain = currentState._4 :+ nextRightCoord
      (culledMap(nextLeftCoord), culledMap(nextRightCoord), leftChain, rightChain, currentState._5 + 1)
    }).find(state => {
      state._1 == state._2
    }).get

    val (halfWayPipe, halfWayPipe2, leftChain, rightChain, lengthToHalf) = finalState
    (halfWayPipe, leftChain, rightChain, lengthToHalf)
  }

  def paintNodes(input: Map[Coord, Pipe], coordsInLoop: Set[Coord], fullPath: Seq[Coord], maxX: Int, maxY: Int) = {

    val coordsAndVectors = fullPath.sliding(2, 1).map { case Seq(point, next) => {
      point -> Vector(next.x - point.x, next.y - point.y)
    }
    }.toSeq

    val coordsNotOnLoop = input.filter(x => !coordsInLoop.contains(x._1)).map(kv => kv._1 -> 0).toMap


    val painted = coordsAndVectors.sliding(2, 1).foldLeft(coordsNotOnLoop)((acc, currentAndNextVector) => {
      val current = currentAndNextVector(0)
      val next = currentAndNextVector(1)

      val vectorToApply = current._2.getInOutDirection()
      val affectedCoords = Iterator.iterate(vectorToApply.apply(current._1))(c => {
        vectorToApply(c)
      }).takeWhile(c => coordsNotOnLoop.contains(c)).toSeq.filter(x => !coordsInLoop.contains(x))

      val allAffected = if (next._2 != current._2) {
        //there is a turn, need to add in the pre-turn applying this vector to the next coord
        affectedCoords ++ Iterator.iterate(vectorToApply.apply(next._1))(c => {
          vectorToApply(c)
        }).takeWhile(c => coordsNotOnLoop.contains(c)).toSeq.filter(x => !coordsInLoop.contains(x))
      } else {
        affectedCoords
      }

      val updatedAcc = allAffected.foldLeft(acc)((acc2, affectedCoord) => {
        acc2.updatedWith(affectedCoord) {
          case Some(currentCount) => Some(currentCount + 1)
          case None => Some(1)
        }
      })

      //      printMap2(input, updatedAcc, maxX, maxY)

      updatedAcc
    })
    val enclosedCoords = painted.filter(kv => kv._2 > 0 && kv._2 % 2 == 0)
    enclosedCoords
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

    //    def offTheMap(maxX: Int, maxY: Int): Boolean = {
    //      x < 0 || y < 0 || x > maxX || y > maxY
    //    }
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

  def printMap2(map: Map[Coord, Pipe], spots: Map[Coord, Int], xMax: Int, yMax: Int): Unit = {
    (0 until yMax).foreach(y => {
      (0 until xMax).foreach(x => {
        print(spots.get(Coord(x, y)).map(_.toString).getOrElse(map(Coord(x, y)).shape))
        //        print(map.get(Coord(x, y)).map(_.shape).getOrElse(spots.get(Coord(x,y)).getOrElse('?')))
      })
      println
    })
    println("---------------------------")
  }
}
