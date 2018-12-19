package de.emiliomg.adventofcode.y2018.day7

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

/**
  * https://adventofcode.com/2018/day/7
  */
object Day7 extends App {

  val testData = parseData(getData("2018/7/test.txt"))

  println(firstStar(testData))
//  secondStar(testData)

  assert(firstStar(testData) == "CABDFE")
//  assert(secondStar(testData) == ???)

  val data = parseData(getData("2018/7/input.txt"))

  val firstStarResult = firstStar(data)
//  val secondStarResult = secondStar(data)

  println(s"Result first star: $firstStarResult")
//  println(s"Result second star: $secondStarResult")

  assert(firstStarResult == "BCADPVTJFZNRWXHEKSQLUYGMIO")
//  assert(secondStarResult == ???)

  def firstStar(data: Set[Node]) = {
    getNodeProcessingOrder(data)
  }

//  def secondStar(data: ???) = {
//    ???
//  }

  def getNodeProcessingOrder(data: Set[Node]) = {
    @tailrec
    def step(graph: Set[Node], checkMe: Set[Char], acc: Array[Char]): String = {
      checkMe.toList.sorted.headOption match {
        case Some(current) ⇒
          val currentNode = getNodeByName(graph)(current)
          val newGraph = graph - currentNode + currentNode.copy(isVisited = true)

          val newNodesToProcess: Set[Char] =
            currentNode.next.map(getNodeByName(newGraph))
              .filter(node ⇒ node.previous.forall(n ⇒ getNodeByName(newGraph)(n).isVisited))
              .map(_.name)

          val newCheckMe = checkMe - current ++ newNodesToProcess
          val newAcc = acc :+ current

          step(newGraph, newCheckMe, newAcc)
        case None ⇒ acc.mkString
      }
    }

    step(data, data.filter(_.previous.isEmpty).map(_.name), Array())
  }

  def getNodeByName(data: Set[Node])(name: Char): Node = data.find(_.name == name).get

  def getData(path: String): List[String] = Source.fromResource(path).getLines().toList
  def parseData(data: List[String]): Set[Node] = data.foldLeft(Set[Node]())((acc, row) ⇒ parseRow(row, acc))

  def parseRow(row: String, acc: Set[Node]): Set[Node] = row match {
    case Node.pattern(firstNameString, secondNameString) ⇒
      val firstName = firstNameString.charAt(0)
      val secondName = secondNameString.charAt(0)
      val first = acc.find(_.name == firstName).getOrElse(Node(firstName))
      val second = acc.find(_.name == secondName).getOrElse(Node(secondName))

      acc.filterNot(n ⇒ List(firstName, secondName).contains(n.name)) + first.addNext(second.name) + second.addPrevious(first.name)
    case nope ⇒ throw new Exception(s"Found $nope, cannot match!")
  }
}

case class Node(name: Char, previous: Set[Char], next: Set[Char], isVisited: Boolean) {
  def addPrevious(name: Char): Node = copy(previous = this.previous + name)
  def addNext(name: Char): Node = copy(next = this.next + name)
}

object Node {
  val pattern: Regex = "Step (.) must be finished before step (.) can begin.".r
  def apply(name: Char): Node = Node(name, Set.empty, Set.empty, isVisited = false)
}
