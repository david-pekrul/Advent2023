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

    CamelCardHand("2233J".toCharArray, 1).handRank2

    val sortedPart2 = parsed.sortWith((a, b) => a.sortsBefore(b)).zipWithIndex.map(x => (x._1, x._2 + 1))
    val part2 = sortedPart2.map(x => x._2 * x._1.bet).sum
    println(s"Part 2 : $part2")
  }

  case class CamelCardHand(cards: Seq[Char], bet: Int) extends Ordered[CamelCardHand] {

    lazy val handRank: Rank.Value = _handRank()
    lazy val handRank2: Rank.Value = _handRank2()

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

    private def _handRank2(): Rank.Value = {

      if (!this.cards.contains('J')) {
        return _handRank()
      }

      val cardCounts = cards.groupBy(a => a).map(x => x._1 -> x._2.size)
      val setCountsNoJ = cardCounts.filter(_._1 != 'J').values.toSeq
      val setCountsNoJMax = setCountsNoJ.maxOption.getOrElse(0)
      val jCount = cardCounts('J')
      if (setCountsNoJMax + jCount == 5) {
        return Rank.FIVE_OF_A_KIND
      }
      if (setCountsNoJMax + jCount == 4) {
        return Rank.FOUR_OF_A_KIND
      }
      if (setCountsNoJ.size == 2 && jCount == 1) {
        return Rank.FULL_HOUSE
      }
      if (setCountsNoJMax + jCount == 3) {
        return Rank.THREE_OF_A_KIND
      }
      if (setCountsNoJ.intersect(Seq(2, 2)).size == 2) {
        return Rank.TWO_PAIR
      }
      if (setCountsNoJMax + jCount == 2) {
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

    def sortsBefore(that: CamelCardHand): Boolean = {
      val rankCompare = this.handRank2.compareTo(that.handRank2)
      val x = if (rankCompare != 0) {
        rankCompare
      } else {
        this.cards.zip(that.cards)
          .map { case (a, b) => Integer.compare(Strength2(a), Strength2(b)) }
          .find(n => n != 0).get
      }
      x < 0
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
  val Strength2: Map[Char, Int] = "A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J".split(",").map(_.trim.charAt(0)).reverse.zipWithIndex.toMap
}
