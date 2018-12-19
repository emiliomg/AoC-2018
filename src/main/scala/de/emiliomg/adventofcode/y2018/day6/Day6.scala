package de.emiliomg.adventofcode.y2018.day6

import scala.io.Source
import scala.util.matching.Regex

/**
  * https://adventofcode.com/2018/day/6
  */
object Day6 extends App {

  def parseRow(data: String): Point = data match {
    case Point.pattern(x, y) ⇒ Point(x.toInt, y.toInt)
    case nope ⇒ throw new Exception(s"Found $nope, cannot match!")
  }

  val testData = parseData[Point](getData("2018/6/test.txt"))(parseRow)

  assert(firstStar(testData) == 17)
  assert(secondStar(testData, 32) == 16)

  val data = parseData(getData("2018/6/input.txt"))(parseRow)

  val firstStarResult = firstStar(data)
  val secondStarResult = secondStar(data, 10000)

  println(s"Result first star: $firstStarResult")
  println(s"Result second star: $secondStarResult")

  assert(firstStarResult == 5532)
  assert(secondStarResult == 36216)

  def firstStar(providedPoints: List[Point]): Int = {
    largestFiniteArea(providedPoints)
  }

  def secondStar(providedPoints: List[Point], maxDistance: Int): Int = {
    largestAreaSizeWithMaxDistanceSum(maxDistance, providedPoints)
  }

  def largestAreaSizeWithMaxDistanceSum(maxDistance: Int, providedPoints: List[Point]): Int = {
    def getDistanceSumToProvicedPoints(pos: Point): Int = providedPoints.map(_.manhattanDistanceTo(pos)).sum

    val grid = Grid(providedPoints)
    val region: Iterator[Int] = for {
      pos ← grid.iterateGridPoints
      dist = getDistanceSumToProvicedPoints(pos)
      if dist < maxDistance
    } yield dist

    region.size
  }

  def largestFiniteArea(providedPoints: List[Point]): Int = {
    def getClosestPointForPosition(pos: Point): Option[Point] = {
      val closestPoints: List[Point] = providedPoints.groupBy(p ⇒ p.manhattanDistanceTo(pos)).minBy(_._1)._2
      if (closestPoints.length == 1) Some(closestPoints.head)
      else None
    }

    val grid = Grid(providedPoints)

    // => Map(gridPosition -> providedDataPoint)
    val processedGrid: Map[Point, Point] = (for {
      pos ← grid.iterateGridPoints
      closest ← getClosestPointForPosition(pos)
    } yield pos → closest).toMap

    val finiteGrid: Map[Point, Point] = {
      val pointsWithInfiniteAreas: Set[Point] = processedGrid.filterKeys(p ⇒ p.x == grid.min.x || p.x == grid.max.x || p.y == grid.min.y || p.y == grid.max.y).groupBy(_._2).keys.toSet
      processedGrid.filter(p ⇒ !pointsWithInfiniteAreas.contains(p._2))
    }

    val providedPointsAreaSizes = finiteGrid.groupBy(_._2).mapValues(_.size)

    providedPointsAreaSizes.values.max
  }

  def getData(path: String): List[String] = Source.fromResource(path).getLines().toList
  def parseData[A](data: List[String])(f: String ⇒ A): List[A] = data.map(x ⇒ f(x))

}

// ------

case class Point(x: Int, y: Int) {
  def manhattanDistanceTo(that: Point): Int = Math.abs(x - that.x) + Math.abs(y - that.y)
}

object Point {
  val pattern: Regex = "([0-9]+), ([0-9]+)".r
}

case class Grid(min: Point, max: Point) {
  def iterateGridPoints: Iterator[Point] = for {
    x ← (min.x to max.x).toIterator
    y ← (min.y to max.y).toIterator
  } yield Point(x, y)
}

object Grid {
  def apply(points: List[Point]): Grid = {
    val xMin: Int = points.minBy(_.x).x
    val xMax: Int = points.maxBy(_.x).x
    val yMin: Int = points.minBy(_.y).y
    val yMax: Int = points.maxBy(_.y).y

    Grid(Point(xMin, yMin), Point(xMax, yMax))
  }
}
