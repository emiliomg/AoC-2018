package de.emiliomg.adventofcode.y2018.day2.day3

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day3 extends App {
  type ClaimID = Int

  val testData = parseData(getData("2018/3/test.txt"))
  assert(testData.length == 3)

  assert(findOverlappingSquareCount(testData) == 4)

  val input = parseData(getData("2018/3/input.txt"))
  assert(input.length == 1385)

  def firstStar = findOverlappingSquareCount(input)
  println(s"first star: $firstStar")

  assert(firstStar == 116491)


  def findOverlappingSquareCount(data: List[FabricClaim]): Int = {
    val squarePositionCounts: Map[Position, List[ClaimID]] = processFabricClaims(data)

    val collisions = squarePositionCounts
      .filter { case (_, claims) ⇒ claims.length > 1}
      .keys

    collisions.size
  }

  def processFabricClaims(data: List[FabricClaim]): Map[Position, List[ClaimID]] = {
    val allSquares: List[(Position, ClaimID)] = data.flatMap {fc ⇒
      for {
        x ← fc.position.x until (fc.position.x + fc.width) // until, not to!
        y ← fc.position.y until (fc.position.y + fc.height)

      } yield (Position(x, y), fc.id)
    }

    allSquares
      .groupBy(posWithID ⇒ Position(posWithID._1.x, posWithID._1.y))
      .mapValues(_.map(posWithID ⇒ posWithID._2))
  }

  def getData(path: String): String = Source.fromResource(path).getLines().mkString("\n")
  def parseData(data: String): List[FabricClaim] = FabricClaimParser.parse(FabricClaimParser.fabricClaims, data).get
}

case class Position(x: Int, y: Int)
case class FabricClaim(id: Int, position: Position, width: Int, height: Int)

object FabricClaimParser extends RegexParsers {
  override protected val whiteSpace: Regex = "[ \t]".r

  def id: Parser[Int] = "#" ~> "[0-9]+".r ^^ (id ⇒ id.toInt)
  def coordinates: Parser[(Int, Int)] = "[0-9]+".r ~ "," ~ "[0-9]+".r ^^{ case x ~ _ ~ y ⇒ x.toInt → y.toInt}
  def dimensions: Parser[(Int, Int)] = "[0-9]+".r ~ "x" ~ "[0-9]+".r ^^{ case width ~ _ ~ height ⇒ width.toInt → height.toInt}

  def fabricClaim: Parser[FabricClaim] = id ~ "@" ~ coordinates ~ ":" ~ dimensions ^^ {
    case id ~ _ ~ coords ~ _ ~ dim ⇒ FabricClaim(id, Position(coords._1, coords._2), dim._1, dim._2)
  }

  def fabricClaims: Parser[List[FabricClaim]] = (fabricClaim <~ "\n".?).*
}
