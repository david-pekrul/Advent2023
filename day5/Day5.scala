package day5

import helpers.Helpers

object Day5 {
  def main(args: Array[String]): Unit = {
    //    val input = Helpers.readFile("day5/test.txt")
    val input = Helpers.readFile("day5/day5.txt")
    val (seeds, lookup) = parse(input)

    val translatedSeeds = seeds.map(seed => (seed, lookup.translate(seed)))

    val part1 = translatedSeeds.map(_._2).min
    println(s"Part 1: $part1")

    val part2SeedRanges = createSeedRanges(seeds)
    val part2Processed = part2SeedRanges.map(x => lookup.translateRange(x))
    val part2 = part2Processed.flatten.map(_.start).min
    println(s"Part 2: $part2")
  }

  def parse(input: Seq[String]) = {
    val itr = input.iterator
    val seedsLine = itr.next().replaceAll(".*:", "").trim
    val seedValues = seedsLine.trim.split("\\s").map(_.toLong).toSeq


    itr.next() //blank line after seeds
    val seedToSoilLine = getNextLines(itr)
    val soilToFertLine = getNextLines(itr)
    val fertToWaterLine = getNextLines(itr)
    val waterToLight = getNextLines(itr)
    val lightToTemp = getNextLines(itr)
    val tempToHumid = getNextLines(itr)
    val humidToLocation = getNextLines(itr)

    val rawStrings = Seq(seedToSoilLine, soilToFertLine, fertToWaterLine, waterToLight, lightToTemp, tempToHumid, humidToLocation)
    (seedValues, Lookup(rawStrings.map(rawStringSeq => LookupTable(rawStringSeq.map(LookupEntry.parse)))))
  }

  def createSeedRanges(seeds: Seq[Long]): Seq[Range2] = {
    seeds.grouped(2).map(pair => Range2(pair.head, pair.head + pair.last)).toSeq
  }

  def getNextLines(itr: Iterator[String]): Seq[String] = {
    itr.takeWhile(s => !s.trim.isEmpty).toSeq.filter(!_.contains(":"))
  }

  case class Lookup(tables: Seq[LookupTable]) {
    def translate(input: Long): Long = {
      tables.foldLeft(input)((acc, nextTable) => {
        nextTable.translate(acc)
      })
    }

    def translateRange(input: Range2): Seq[Range2] = {
      tables.foldLeft(Seq(input))((inputRangesFromPreviousTable, nextTable) => {
        val translationsFromTable = inputRangesFromPreviousTable.flatMap(r => nextTable.translateRange(r))
        translationsFromTable
      })
    }

  }

  case class LookupTable(entries: Seq[LookupEntry]) {

    lazy val entriesSortedbySourceStart = entries.sortBy(a => a.sourceStart)

    def translate(input: Long): Long = {
      val firstPass = entriesSortedbySourceStart.flatMap(_.translate(input)).headOption
      firstPass match {
        case Some(translatedValue) => translatedValue
        case None => input
      }
    }

    def translateRange(input: Range2): Seq[Range2] = {

      //for each untranslated range, fold left through all of the lookup entries
      val initialState = (Seq(input), Seq[Range2]()) //untranslated : translated
      val (unmodifiedRanges,modifiedRanges) = entriesSortedbySourceStart.foldLeft(initialState)((acc, currentLookupEntry) => {
        val currentUntranslated = acc._1
        val alreadyTranslatedRanges = acc._2
        if (currentUntranslated.isEmpty) {
          acc //no more to translate
        } else {
          //take each untranslated and attempt to translate them through the currentLookupEntry
          val translationsThroughLookupEntry = currentUntranslated.map(r => currentLookupEntry.translateRange(r))
          val remainingUntranslated = translationsThroughLookupEntry.map(_._1).flatten
          val newTranslations = translationsThroughLookupEntry.map(_._2).flatten
          (remainingUntranslated, newTranslations ++ alreadyTranslatedRanges)
        }
      })
      //take all the ranges that didn't get translated and append the ranges that did get translated
      unmodifiedRanges ++ modifiedRanges
    }


  }

  case class LookupEntry(destinationStart: Long, sourceStart: Long, rangeLength: Long) {

    val sourceEnd: Long = sourceStart + rangeLength - 1 //inclusive
    val destinationEnd: Long = destinationStart + rangeLength - 1

    override def toString: String = {
      s"source[$sourceStart, $sourceEnd] -> dest[$destinationStart,$destinationEnd] (range:$rangeLength)"
    }

    def translate(input: Long): Option[Long] = {
      //ranges are inclusive-exclusive [start,end)
      if (sourceStart <= input && input < sourceStart + rangeLength) {
        //This Entry will translate this input
        val offset: Long = input - sourceStart
        val output: Long = destinationStart + offset
        Some(output)
      } else {
        None
      }
    }

    /**
     * There are 6 cases to consider when doing range splits
     * ..............input |------------------|
     * This entry: [-1-][--2--][---3---][---4--][-5-]
     * __________________[-----------6-----------]
     * 1: this entry ends before the input starts, having the input unchanged
     * 2: this entry overlaps the beginning part of the input, causing an output of 2 ranges
     * 3: this entry is fully contained in the input, causing an output of 3 ranges
     * 4: this entry overlaps the end part of the input, causing an output of 2 ranges
     * 5: this entry starts after the input ends, having the input unchanged
     * 6: this entry fully contains the input, having the output of 1 fully translated input
     */
    //Return: (Remaining source Range(s), Translated Ranges)
    def translateRange(input: Range2): (Seq[Range2], Seq[Range2]) = {

      //case 5
      if (input.end < this.sourceStart) {
        return (Seq(input), Seq()) //this was untranslated, and no translation happened
      }

      //case 1
      if (this.sourceEnd < input.start) {
        return (Seq(input), Seq()) //this was untranslated, and no translation happened
      }

      //case 6
      if (input.start >= this.sourceStart && input.end <= this.sourceEnd) {
        val shift = this.sourceStart - this.destinationStart
        val wholeUpdatedRange = Range2(input.start - shift, input.end - shift)
        return (Seq(), Seq(wholeUpdatedRange)) //the whole input was shifted, and nothing remained
      }

      //case 3
      if (input.start < this.sourceStart && this.sourceEnd < input.end) {
        //left, middle, right
        val left = Range2(input.start, this.sourceStart - 1)
        val right = Range2(this.sourceEnd + 1, input.end)
        val middle = this
        val shift = this.sourceStart - this.destinationStart
        val middleShifted = Range2(middle.sourceStart - shift, middle.sourceEnd - shift)
        return (Seq(left, right), Seq(middleShifted)) //the left and right are not modified, but the middle was
      }

      //case 2
      if (this.sourceStart <= input.start && this.sourceEnd <= input.end) {
        //left,right
        val right = Range2(this.sourceEnd + 1, input.end)
        val leftToShift = Range2(input.start, this.sourceEnd)
        val shift = this.sourceStart - this.destinationStart
        val leftShifted = Range2(leftToShift.start - shift, leftToShift.end - shift)
        return (Seq(right), Seq(leftShifted)) //the right was not modified, but the left was
      }


      //case 4
      if (input.start <= this.sourceStart && input.end <= this.sourceEnd) {
        //left, right
        val left = Range2(input.start, this.sourceStart - 1)
        val rightToShift = Range2(this.sourceStart, input.end)
        val shift = this.sourceStart - this.destinationStart
        val rightShifted = Range2(rightToShift.start - shift, rightToShift.end - shift)
        return (Seq(left), Seq(rightShifted)) //left was not modified, but the right was
      }

      // Uh...... whoops!
      println(s"This: $this\r\nRange2: $input")
      ???
    }
  }

  object LookupEntry {
    val regex = """^(\d+) (\d+) (\d+)""".r

    def parse(input: String): LookupEntry = {
      val regex(dest, src, r) = input
      LookupEntry(dest.toLong, src.toLong, r.toLong)
    }

  }

  //Inclusive ends
  case class Range2(start: Long, end: Long) {}
}
