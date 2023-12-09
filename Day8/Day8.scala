package Day8

import helpers.Helpers

object Day8 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day8/test.txt")
//    val input = Helpers.readFile("day8/test2.txt")
    val input = Helpers.readFile("day8/day8.txt")

    val (instructions, nodes) = parse(input)

    val loopingIndexedInstructions = Helpers.infiniteStream(instructions)

    val nodeLookup = nodes.map(node => node.id -> node).toMap

    val part1 = process(loopingIndexedInstructions, nodeLookup)
    println(s"Part 1 : $part1")

  }

  def process(instructions: LazyList[(Char)], nodeLookup: Map[String, Node]): Long = {
    instructions.foldLeft((nodeLookup("AAA"), 1L))((acc, nextInstruction) => {
      val previousNode = acc._1
      val stepCount = acc._2
      val nextNode = nodeLookup(previousNode.next(nextInstruction))
      if (nextNode.id.equals("ZZZ")) {
        return stepCount //indexes start at 0
      }
      (nextNode, stepCount + 1)
    })

    -9
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
