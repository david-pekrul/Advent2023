package day12

import helpers.Helpers

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Helpers.readFile("day12/test.txt")
    //    val input = Helpers.readFile("day12/day12.txt")
    val parsed = parse(input)
    //    parsed.foreach(println)

    //    val test1 = parse(Seq("???.### 1,1,3")).head
    //    test1.count()

    //    val things = parsed.map(x => (x.count(), x))
    //    things.foreach(println)


//    val test = parse(Seq("???.### 1,1,3")).head
    val test = parse(Seq(".??..??...?##. 1,1,3")).head
    //    val test = parse(Seq("?###???????? 3,2,1")).head
    //    val test = parse(Seq("## 5")).head

//    val testSmear = smear(test.brokenCounts.head, 0, test.row)
    val testSmear = smearBig(test)
    testSmear.foreach(println)


//    val part1 = parsed.map(x => x -> smearBig(x).size)
//    part1.foreach(println)

//    println(s"Part 1: $part1")
  }

  def parse(input: Seq[String]): Seq[SpringRow] = {
    val springRegex = """^([#\.\?]+) ([\d,]+)""".r

    input.map(line => {
      val springRegex(symbols, counts) = line
      //      val condensedRow = symbols.replaceAll("""\.+""",".") //consecutive working springs don't matter
      val trimmed = trimString(symbols)
      val brokenCounts = counts.split(",").map(_.toInt).toSeq
      SpringRow(trimmed, brokenCounts)
    })
  }

  val trimmingRegex = """^\.*(.+?)\.*$""".r
  val condensingRegex = """(\.+)"""

  def trimString(input: String): Seq[Char] = {
    val trimmingRegex(trim) = input
    trim.replaceAll(condensingRegex, ".").toSeq
  }

  def trimChars(input: Seq[Char]): Seq[Char] = {
    input.dropWhile(_ == '.')
  }

  def smearBig(springRow: SpringRow) = {

    val startingState2 = smear(springRow.brokenCounts.head,0,springRow.row)

    springRow.brokenCounts.tail.foldLeft(startingState2)((acc,nextBrokenCount) => {
      acc.map(prevRowState => {
        val thing = prevRowState -> smear(nextBrokenCount,0,prevRowState._2)
//        println(thing)
        thing._2
      }).flatten
    })


  }

  /**
   *
   * @param blockSize
   * @param startingIndex
   * @param symbols
   * @return
   */
  def smear(blockSize: Int, startingIndex: Int, symbols: Seq[Char]) = {
    val symbolsWithIndexAndPoundCount = symbols.zipWithIndex.map(x => (x._1, x._2, Int.MaxValue)).foldLeft((Seq[(Char, Int, Int)](), 0))((acc, next) => {
      val runningTotal = acc._2 + (if (next._1 == '#') {
        1
      } else {
        0
      })
      val recomposed = (next._1, next._2, runningTotal)
      (acc._1 :+ recomposed, runningTotal)
    })._1

    val rebuilt = if (symbolsWithIndexAndPoundCount.size == 1) {
      symbolsWithIndexAndPoundCount
    } else {
      symbolsWithIndexAndPoundCount.take(1) ++ symbolsWithIndexAndPoundCount.reverse.sliding(2, 1).map(reversedPair => {
        val right = reversedPair(0)
        val left = reversedPair(1)

        (right._1, right._2, left._3)
      }).toSeq.reverse
    }


    //Seq[(Char symbol, index in string, number of # symbols to the left of this position)]
    //    println(rebuilt)

    val groupsUntilDotOrTooManyPounds = rebuilt.drop(startingIndex).sliding(blockSize, 1).takeWhile(currentGroup => {
      //does the current group work for the block size?
      if (currentGroup.exists(charInt => charInt._1 == '.')) {
        //there is a working spring in the block of broken springs -> can't work
        false
      } else if (currentGroup.last._3 >= blockSize && currentGroup.last._2 >= blockSize) {
        //this cuts it off one too early....
        false
      } else if (currentGroup.size < blockSize) {
        false //the group is too small for the block size
      }
      else {
        //todo: filter out later if the next char is a #, which would mean this group butts up against another without a gap
        true
      }
    }).toSeq


    //still need to check if any of the returned options butt up against another # symbol...

    //todo: need to pair each option up with the remaining (and maybe modified) symbols.
    // the next symbol might be forced from a ? to a . (aka: removed)
    // after I do this, do I really need to keep track of the indexes of each char?

    val filteredByNextChar = groupsUntilDotOrTooManyPounds.map(groupedInfo => {
      val startingIndex = groupedInfo(0)._2
      val nextCharIsPound = symbols.lift(startingIndex + blockSize).map(c => c == '#').getOrElse(false)
      if (nextCharIsPound) {
        None
      } else {
        Some((groupedInfo, symbols.drop(startingIndex + blockSize + 1))) //the + 1 is because two blocks can't be next to each other, so this symbol, whether a . or a ? must be dropped
      }
    }).filter(_.isDefined).map(_.get)

    //    filteredByNextChar.foreach(println)


    //Seq[
    // symbols string
    // ,
    // Seq of remaining symbols after taking the other ones
    // ,
    // startingIndex
    // ]
    val condensed = filteredByNextChar.map(x => {
      (x._1.map(y => y._1).mkString(""), x._2, x._1.map(_._2).min)
    })

    condensed
  }

  case class SpringRow(row: Seq[Char], brokenCounts: Seq[Int]) {

    def isEvenPossible(): Boolean = {
      if (row.isEmpty) {
        return false
      }
      if (row.size < brokenCounts.size + brokenCounts.sum - 1) {
        return false
      }

      true
    }
  }

  def satisfies(input: Seq[Char], brokenCount: Int): Boolean = {
    if (input.size < brokenCount) {
      //input isn't big enough to hold this many broken springs
      return false
    }
    if (input.count(_ == '#') > brokenCount) {
      //too many broken springs in this input
      return false
    }

    true
  }
}
