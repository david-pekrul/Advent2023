package day7

import helpers.Helpers

object Day7 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day7/test.txt")
    val input = Helpers.readFile("day7/day7.txt")
    val parsed = parse(input)

    val fullySorted = parsed.sorted.zipWithIndex.map(x => (x._1, x._2 + 1))
    val part1 = fullySorted.map(x => x._2 * x._1.bet).sum
    println(s"Part 1 : $part1")
  }

  case class CamelCardHand(cards: Seq[Char], bet: Int) extends Ordered[CamelCardHand] {

    lazy val handRank: Rank.Value = _handRank()

    private def _handRank(): Rank.Value = {
      val setCounts = cards.groupBy(a => a).map(x => x._1 -> x._2.size).values.toSeq
      if (setCounts.max == 5) {
        return Rank.FIVE_OF_A_KIND
      }
      if (setCounts.max == 4) {
        return Rank.FOUR_OF_A_KIND
      }
      if (setCounts.size == 2) {
        return Rank.FULL_HOUSE
      }
      if (setCounts.max == 3) {
        return Rank.THREE_OF_A_KIND
      }
      if (setCounts.intersect(Seq(2, 2)).size == 2) {
        return Rank.TWO_PAIR
      }
      if (setCounts.max == 2) {
        return Rank.ONE_PAIR
      }
      return Rank.HIGH_CARD
    }

    override def compare(that: CamelCardHand): Int = {
      val rankCompare = this.handRank.compareTo(that.handRank)
      if (rankCompare != 0) {
        rankCompare
      } else {
        this.cards.zip(that.cards)
          .map { case (a, b) => Integer.compare(Strength(a), Strength(b)) }
          .find(n => n != 0).get
      }
    }
  }

  def parse(input: Seq[String]): Seq[CamelCardHand] = {
    val cardRegex = """^(\w{5})\s(\d+)$""".r
    input.map(line => {
      val cardRegex(cards, bet) = line
      CamelCardHand(cards.toCharArray, bet.toInt)
    })
  }

  object Rank extends Enumeration {
    val FIVE_OF_A_KIND = Value(7)
    val FOUR_OF_A_KIND = Value(6)
    val FULL_HOUSE = Value(5)
    val THREE_OF_A_KIND = Value(4)
    val TWO_PAIR = Value(3)
    val ONE_PAIR = Value(2)
    val HIGH_CARD = Value(1)
  }

  val Strength: Map[Char, Int] = "A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2".split(",").map(_.trim.charAt(0)).reverse.zipWithIndex.toMap
}
