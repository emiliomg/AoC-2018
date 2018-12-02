package de.emiliomg.adventofcode.y2018.day1

import scala.io.Source

object Day1 extends App {

  type FrequencyShift = Int

  val input: List[FrequencyShift] = parseData(getData("2018/1/input.txt"))
  assert(input.length == 1029)

  assert(sumFrequencyShifts(parseData("+1, +1, +1")) == 3)
  assert(sumFrequencyShifts(parseData("+1, +1, -2")) == 0)
  assert(sumFrequencyShifts(parseData("-1, -2, -3")) == -6)

  assert(findFirstRevisitedFrequency(parseData("+1, -1")) == 0)
  assert(findFirstRevisitedFrequency(parseData("+3, +3, +4, -2, -4")) == 10)
  assert(findFirstRevisitedFrequency(parseData("-6, +3, +8, +5, -6")) == 5)
  assert(findFirstRevisitedFrequency(parseData("+7, +7, -2, -7, -4")) == 14)

  val firstStar = sumFrequencyShifts(input)
  println(s"firstStar: $firstStar")
  assert(firstStar == 536)

  val secondStar = findFirstRevisitedFrequency(input)
  println(s"secondStar: $secondStar")
  assert(secondStar == 75108)

  def sumFrequencyShifts(data: List[FrequencyShift]): Int = data.foldRight(0){case (fs, acc) ⇒ acc + fs}

  def findFirstRevisitedFrequency(data: List[FrequencyShift]): Int = {

    def step(partialList: List[FrequencyShift], current: Int, storage: Set[Int]): Int = {
      if (partialList.isEmpty) step(data, current, storage)
      else {
        val nextFrequency: Int = current + partialList.head

        if (storage.contains(nextFrequency)) nextFrequency
        else {
          val newStorage = storage + nextFrequency
          step(partialList.tail, nextFrequency, newStorage)
        }
      }
    }

    step(data, 0,Set(0))
  }

  def getData(path: String): String = Source.fromResource(path).getLines().toList.mkString(", ")
  def parseData(data: String): List[FrequencyShift] = data.split(", ").map(fs ⇒ fs.toInt).toList
}
