package day12

import helpers.Helpers

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Helpers.readFile("day12/test.txt")
    val parsed = parse(input)
    parsed.foreach(println)

    val test1 = parse(Seq("???.### 1,1,3")).head

    test1.count()


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

  def trimString(input: String): Seq[Char] = {
    val trimmingRegex(trim) = input
    trim.toSeq
  }

  case class SpringRow(row: Seq[Char], brokenCounts: Seq[Int]) {

    def count(): Int = {

      def _recCount(currentRow: Seq[Char], remainingBrokenCounts: Seq[Int]): Int = {

        if (remainingBrokenCounts.isEmpty) {
          return 0
        }

        val startState: (SpringRow, SpringRow) = (SpringRow(Seq(), remainingBrokenCounts.take(1)), SpringRow(currentRow, remainingBrokenCounts.tail))

        val firstPass = Iterator.iterate(startState)(prevState => {
          val nextLeft = SpringRow(prevState._1.row :+ prevState._2.row.head, prevState._1.brokenCounts)
          val nextRight = SpringRow(prevState._2.row.tail, prevState._2.brokenCounts)
          (nextLeft, nextRight)
        }).takeWhile(_._2.isEvenPossible()).filter(_._1.isEvenPossible()).toSeq

        println(firstPass)

        val updatedPass = firstPass.map{ case (left,right) => {
          val updatedRight = if(right.row.head=='?'){
            SpringRow(right.row.tail,right.brokenCounts)
          } else {
            right
          }
          (left,updatedRight)
        }}.filter(x => x._1.isEvenPossible() && x._2.isEvenPossible())

        println(updatedPass)

        3
      }

      _recCount(row, brokenCounts)
      ???
    }

    def isEvenPossible(): Boolean = {
      if (row.isEmpty && !brokenCounts.isEmpty) {
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
