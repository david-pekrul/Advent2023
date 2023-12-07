package day5

import helpers.Helpers

object Day5 {
  def main(args: Array[String]): Unit = {
    //        val input = Helpers.readFile("day5/test.txt")
    val input = Helpers.readFile("day5/day5.txt")
    val (seeds, lookup) = parse(input)

    val translatedSeeds = seeds.map(seed => (seed, lookup.translate(seed)))

    println(translatedSeeds)

    val part1 = translatedSeeds.map(_._2).min
    println(s"Part 1: $part1")

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

  def getNextLines(itr: Iterator[String]): Seq[String] = {
    itr.takeWhile(s => !s.trim.isEmpty).toSeq.filter(!_.contains(":"))
  }

  case class Lookup(tables: Seq[LookupTable]) {
    def translate(input: Long): Long = {
      tables.foldLeft(input)((acc, nextTable) => {
        nextTable.translate(acc)
      })
    }
  }

  case class LookupTable(entries: Seq[LookupEntry]) {

    lazy val entriesSortedbySourceStart = entries.sortBy(a => a.sourceRangeStart)

    def translate(input: Long): Long = {
      val firstPass = entriesSortedbySourceStart.flatMap(_.translate(input)).headOption
      firstPass match {
        case Some(translatedValue) => translatedValue
        case None => input
      }
    }
  }

  case class LookupEntry(destinationRangeStart: Long, sourceRangeStart: Long, rangeLength: Long) {
    def translate(input: Long): Option[Long] = {
      //ranges are inclusive-exclusive [start,end)
      if (sourceRangeStart <= input && input < sourceRangeStart + rangeLength) {
        //This Entry will translate this input
        val offset: Long = input - sourceRangeStart
        val output: Long = destinationRangeStart + offset
        Some(output)
      } else {
        None
      }
    }
  }

  object LookupEntry {
    val regex = """^(\d+) (\d+) (\d+)""".r

    def parse(input: String): LookupEntry = {
      val regex(dest, src, r) = input
      LookupEntry(dest.toLong, src.toLong, r.toLong)
    }
  }
}
