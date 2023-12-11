package day1

import helpers.Helpers

object Day1 {
  def main(args: Array[String]): Unit = {
//        val input = Helpers.readFile("day1/test.txt")
//        val input = Helpers.readFile("day1/test1_1.txt")
    val input = Helpers.readFile("day1/day1.txt")

    val twoDigits = input
      .map(line => {
        line.replaceAll("[a-zA-Z]", "")
      })
      .map(_.toCharArray.toSeq)
      .map(digits => {
        charToInt(digits.headOption.getOrElse('0')) * 10 + charToInt(digits.lastOption.getOrElse('0'))
      })

    val part1 = twoDigits.sum

    println(s"Part 1: $part1") //54953

    val part2 = input.map(unspellNumbers).sum

    println(s"Part 2: $part2")
  }

  implicit def stringToInt(x: String) = Integer.parseInt(x)

  def charToInt(x: Char): Int = {
    Integer.parseInt("" + x)
  }

  def unspellNumbers(input: String): Int = {

    val firstDigitAndIndex = input.toCharArray.zipWithIndex.find(kv => digits.contains("" + kv._1)).map(x => (charToInt(x._1), x._2)).getOrElse((1000,input.length-1))
    val lastDigitAndIndex = input.toCharArray.zipWithIndex.findLast(kv => digits.contains("" + kv._1)).map(x => (charToInt(x._1), x._2)).getOrElse((1000,0))

    val firstfoundWord = (0 to firstDigitAndIndex._2)
      .flatMap(startIndex => {
        wordToNumber.map(kv => {
          (input.substring(startIndex).startsWith(kv._1), startIndex, kv._2)
        })
      })
      .filter(_._1)
      .map(x => (x._2, x._3))

    val lastFoundWord = (input.length to lastDigitAndIndex._2 by -1)
      .flatMap(endIndex => {
        wordToNumber.map(kv => {
          (input.substring(endIndex).startsWith(kv._1), endIndex, kv._2)
        })
      })
      .filter(_._1)
      .map(x => (x._2, x._3))

    val firstValue = firstfoundWord.map(_._2.intValue()).headOption.getOrElse(firstDigitAndIndex._1)
    val lastValue = lastFoundWord.map(_._2.intValue()).headOption.getOrElse(lastDigitAndIndex._1)

    val result = firstValue*10 + lastValue

    result

  }

  val digits = (1 to 9).map("" + _).toSet

  val wordToNumber: Map[String, Integer] = Map(
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  )
}