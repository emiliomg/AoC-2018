package de.emiliomg.adventofcode.y2018.day5

import scala.io.Source

object Day5 extends App {

  type Polymer = List[Char]

  val testData: Polymer = getData("2018/5/test.txt")

  assert(reactPolymer(testData).mkString == "dabCBAcaDA")

  val data: Polymer = getData("2018/5/input.txt")
  val firstStar: Int = reactPolymer(data).length

  println(s"First star: $firstStar")

  assert(firstStar == 9238)

  /**
    dabAcCaCBAcCcaDA  The first 'cC' is removed.
    dabAaCBAcCcaDA    This creates 'Aa', which is removed.
    dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
    dabCBAcaDA        No further actions can be taken.
   */
  def reactPolymer(data: Polymer): Polymer = {
    data.foldRight(List[Char]()){
      case (char, head :: tail) if willReact(char, head) ⇒ tail
      case (char, acc) ⇒ char :: acc
    }
  }

  def willReact(a: Char, b:Char): Boolean = a != b && a.toLower == b.toLower

  def getData(path: String): Polymer = {
    val data = Source.fromResource(path).getLines().toList
    assert(data.length == 1)
    data.head.toList
  }
}
