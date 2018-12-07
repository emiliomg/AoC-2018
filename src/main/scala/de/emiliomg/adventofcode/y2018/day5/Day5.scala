package de.emiliomg.adventofcode.y2018.day5

import scala.io.Source

object Day5 extends App {

  type Polymer = List[Char]

  val testData: Polymer = getData("2018/5/test.txt")

  assert(reactPolymerLength(testData) == "dabCBAcaDA".length)
  assert(bestPolymerLength(testData) == 4)

  val data: Polymer = getData("2018/5/input.txt")
  val firstStar: Int = reactPolymerLength(data)
  val secondStar: Int = bestPolymerLength(data)

  println(s"First star: $firstStar")
  println(s"Second star: $secondStar")

  assert(firstStar == 9238)
  assert(secondStar == 4052)

  def bestPolymerLength(data: Polymer): Int = {
    val polymerUnits = data.map(_.toLower).toSet
    polymerUnits.map(rmUnit ⇒ data.filterNot(_.toLower == rmUnit)).map(reactPolymerLength).min
  }

  /**
    dabAcCaCBAcCcaDA  The first 'cC' is removed.
    dabAaCBAcCcaDA    This creates 'Aa', which is removed.
    dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
    dabCBAcaDA        No further actions can be taken.
   */
  def reactPolymerLength(data: Polymer): Int = {
    data.foldRight(List[Char]()){
      case (char, head :: tail) if willReact(char, head) ⇒ tail
      case (char, acc) ⇒ char :: acc
    }.length
  }

  def willReact(a: Char, b:Char): Boolean = a != b && a.toLower == b.toLower

  def getData(path: String): Polymer = {
    val data = Source.fromResource(path).getLines().toList
    assert(data.length == 1)
    data.head.toList
  }
}
