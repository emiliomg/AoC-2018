package de.emiliomg.adventofcode.y2018.day2

import scala.io.Source

object Day2 extends App {

  val testDataStar1: List[String] = getData("2018/2/testStar1.txt")
  assert(testDataStar1.length == 7)
  assert(findChecksum(testDataStar1) == 12)

  val testDataStar2: List[String] = getData("2018/2/testStar2.txt")
  assert(findSingularCharDiff(testDataStar2) == "fgij")

  val input: List[String] = getData("2018/2/input.txt")

  val firstStar = findChecksum(input)
  println(s"firstStar: $firstStar")

  val secondStar = findSingularCharDiff(input)
  println(s"secondStar: $secondStar")

  assert(firstStar == 5166)
  assert(secondStar == "cypueihajytordkgzxfqplbwn")

  def findChecksum(testData: List[String]): Int = {
    testData.map(
      _.groupBy(identity)
      .mapValues(_.length)
      .filter { case (_, len) ⇒ len == 2 || len == 3 }
    ) // => List(Map(), Map(b -> 3, a -> 2), Map(b -> 2), Map(c -> 3), Map(d -> 2, a -> 2), Map(e -> 2), Map(b -> 3, a -> 3))
    .flatMap(_
      .map(_.swap) // remove duplicates of character counts
      .keys
    ) // => List(3, 2, 2, 3, 2, 2, 3)
    .groupBy(identity)
    .map(_._2.size)
    .product
  }

  def findSingularCharDiff(data: List[String]): String = {
    val checkSize = data.head.length - 1
    val diffingByOneChar = (
      for {
        comb ← data.combinations(2).toList
        first = comb.head
        second = comb.last
      } yield for {
        (a, b) <- first zip second
        if a == b
      } yield (a, b)
    )
      .filterNot(_.isEmpty)
      .filter(tuples ⇒ tuples.length == checkSize)

    if (diffingByOneChar.length != 1) throw new Exception(s"Diff by one not distinct - $diffingByOneChar")

    diffingByOneChar.head.map(_._1).mkString("")
  }

  def getData(path: String): List[String] = Source.fromResource(path).getLines().toList
}
