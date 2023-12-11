package day8

import helpers.Helpers

object Day8 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day8/test.txt")
    //    val input = Helpers.readFile("day8/test1_1.txt")
    //    val input = Helpers.readFile("day8/test3.txt")
    val input = Helpers.readFile("day8/day8.txt")

    val (instructions, nodes) = parse(input)

    val loopingIndexedInstructions = Helpers.infiniteStream(instructions)

    val nodeLookup = nodes.map(node => node.id -> node).toMap

    val part1 = process(loopingIndexedInstructions, nodeLookup)
    println(s"Part 1 : $part1")

    val part2 = process2(loopingIndexedInstructions, nodeLookup)
    println(s"Part 2: $part2")

  }

  def process(instructions: LazyList[(Char)], nodeLookup: Map[String, Node]): Long = {

    val startNode = nodeLookup.getOrElse("AAA", return -1)

    instructions.foldLeft((nodeLookup("AAA"), 1L))((acc, nextInstruction) => {
      val previousNode = acc._1
      val stepCount = acc._2
      val nextNode = nodeLookup(previousNode.next(nextInstruction))
      if (nextNode.id.equals("ZZZ")) {
        return stepCount //indexes start at 0
      }
      (nextNode, stepCount + 1)
    })

    ???
  }

  def process2(instructions: LazyList[(Char)], nodeLookup: Map[String, Node]) = {

    val startingNodes = nodeLookup
      .filter(_._1.endsWith("A"))
      .toSeq
      .map(_._2)

    //loop size , first index
    def _findLoopSize(startNode: Node): (Long, Long) = {

      //seen nodes: Map[Node.Id -> Index seen at]
      //when I find a node that I've seen before, I'll know the first index, and then the loop size
      //I don't want to assume that the starting nodes will hit a Z node in exactly the same loop size
      //  (The start node branch can enter the loop at any point)

      instructions.foldLeft((startNode, 1L, Map[String, Long](startNode.id -> 0)))((acc, nextInstruction) => {
        val previousNode = acc._1
        val stepCount = acc._2
        val seenNodes = acc._3
        val nextNode = nodeLookup(previousNode.next(nextInstruction))
        val updatedMap: Map[String, Long] = seenNodes.get(nextNode.id) match {
          case Some(previousIndex) => {
            if (nextNode.id.endsWith("Z")) {
              return (stepCount - previousIndex, previousIndex)
            }
            seenNodes
          }
          case None => {
            seenNodes + (nextNode.id -> stepCount)
          }
        }
        (nextNode, stepCount + 1, updatedMap)
      })

      ???
    }

    //checked that there are only 1 Z nodes in each loop, and the start and Z always point to the same node
    val loopSizes = startingNodes.map(startNode => _findLoopSize(startNode)).map(_._1)
    //    loopSizes.foreach(println)

    Helpers.lcm(loopSizes)
  }

  def parse(input: Seq[String]) = {
    val itr = input.iterator
    val instructions = itr.next()
    itr.next() //throw away empty line

    val instructionRegex = """^(\w{3}) = \((\w{3}), (\w{3})\)$""".r
    val nodes = itr.map(line => {
      val instructionRegex(id, left, right) = line
      Node(id, left, right)
    }).toSeq

    (instructions.toCharArray.toSeq, nodes)
  }

  case class Node(id: String, left: String, right: String) {
    def next(instruction: Char): String = {
      instruction match {
        case 'L' => left
        case 'R' => right
      }
    }
  }
}
