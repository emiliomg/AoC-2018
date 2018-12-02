package de.emiliomg.adventofcode.y2018.day2

import scala.io.Source

object Day2 extends App {

  val testData: List[String] = getData("2018/2/test.txt")
  assert(testData.length == 7)
  assert(findChecksum(testData) == 12)

  val input: List[String] = getData("2018/2/input.txt")

  val firstStar = findChecksum(input)
  println(s"firstStar: $firstStar")
  assert(firstStar == 5166)

  def findChecksum(testData: List[String]): Int = {
    // => List(Map(), Map(b -> 3, a -> 2), Map(b -> 2), Map(c -> 3), Map(d -> 2, a -> 2), Map(e -> 2), Map(b -> 3, a -> 3))
    val count23 = testData.map(_
      .groupBy(identity)
      .mapValues(_.length)
      .filter{ case (_, len) ⇒ len == 2 || len == 3 }
    )

    count23.flatMap(_
        .map(_.swap)
        .keys
      )
      .groupBy(identity)
      .map(_._2.size)
      .product
  }

  def getData(path: String): List[String] = Source.fromResource(path).getLines().toList
}
