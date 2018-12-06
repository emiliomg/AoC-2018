package de.emiliomg.adventofcode.y2018.day4

import java.time.{LocalDate, LocalDateTime}

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day4 extends App {

  val testData: List[Event] = parseData(getData("2018/4/test.txt")).sortWith((a, b) ⇒ a.ts.isBefore(b.ts))
  val testGuards: List[Guard] = buildGuards(testData)

  assert(firstStrategy(testGuards) == 240)

  val data: List[Event] = parseData(getData("2018/4/input.txt")).sortWith((a, b) ⇒ a.ts.isBefore(b.ts))
  val guards: List[Guard] = buildGuards(data)

  val firstStar = firstStrategy(guards)

  println(s"First Star: $firstStar")

  assert(firstStar == 11367)

  def firstStrategy(guards: List[Guard]): Int = {
    val mostSleepyGuard: Guard = guards.maxBy(_.getAllMinutesAsleep)
    mostSleepyGuard.id * mostSleepyGuard.getMinuteMostSleptInt
  }

  def buildGuards(data: List[Event]): List[Guard] = {
    type GuardMap = Map[Int, Guard]

    def handleStart(events: List[Event], acc: GuardMap): GuardMap = events.head match {
      case Event(_, BeginShift(gid)) ⇒ handleAwakeGuard(events.tail, acc.updated(gid, Guard(gid)), gid)
      case nope: Event ⇒ throw new Exception(s"Event $nope should not happen at 'handleStart'")
    }

    def handleAwakeGuard(events: List[Event], acc: GuardMap, currentGuardID: Int): GuardMap = events.headOption match {
      case None ⇒ acc
      case Some(Event(ts, Sleep)) ⇒ handleSleepingGuard(events.tail, acc, currentGuardID, ts)
      case Some(Event(_, BeginShift(gid))) ⇒
        if (acc.contains(gid)) handleAwakeGuard(events.tail, acc, gid)
        else handleAwakeGuard(events.tail, acc.updated(gid, Guard(gid)), gid)
      case Some(nope: Event) ⇒ throw new Exception(s"Event $nope should not happen at 'handleAwakeGuard'")
    }

    def handleSleepingGuard(events: List[Event], acc: GuardMap, currentGuardID: Int, startSleeping: LocalDateTime): GuardMap = events.head match {
      case Event(ts: LocalDateTime, WakeUp) ⇒
        val day = LocalDate.from(startSleeping)
        val minutes = (startSleeping.getMinute until ts.getMinute).toList
        val newGuard = acc(currentGuardID).addMinutes(day, minutes)
        handleAwakeGuard(events.tail, acc.updated(currentGuardID, newGuard), currentGuardID)
      case nope: Event ⇒ throw new Exception(s"Event $nope should not happen at 'handleSleepingGuard'")
    }

    handleStart(data, Map()).values.toList
  }

  def getData(path: String): String = Source.fromResource(path).getLines().mkString("\n")
  def parseData(data: String): List[Event] = SleepPatternParser.parse(SleepPatternParser.events, data).get
}

case class Guard(id: Int, allMinutesAsleep: Map[LocalDate, List[Int]]) {
  def addMinutes(day: LocalDate, minutes: List[Int]): Guard = {
    val newMinutes = allMinutesAsleep.getOrElse(day, List()) ++ minutes
    Guard(id, allMinutesAsleep.updated(day, newMinutes))
  }

  def getAllMinutesAsleep: Int = {
    allMinutesAsleep.values.flatten.size
  }

  def getMinuteMostSleptInt: Int =
    allMinutesAsleep
      .values
      .flatten
      .groupBy(identity)
      .maxBy{ case (_, list) ⇒ list.size}
      ._1
}
object Guard {
  def apply(gid: Int): Guard = Guard(gid, Map())
}

sealed trait EventType
case class Event(ts: LocalDateTime, kind: EventType)
case class BeginShift(guardID: Int) extends EventType
case object Sleep extends EventType
case object WakeUp extends EventType

object SleepPatternParser extends RegexParsers {

  def timestamp: Parser[LocalDateTime] = "[" ~> "[0-9]{4}-[0-9]{2}-[0-9]{2}".r ~ "[0-9]{2}:[0-9]{2}".r <~ "]" ^^ {
    case date ~ time ⇒ LocalDateTime.parse(s"${date}T$time:00")
  }

  def beginShift: Parser[BeginShift] = "Guard" ~ "#" ~> "[0-9]+".r <~ "begins shift" ^^ (id ⇒ BeginShift(id.toInt))
  def sleep: Parser[Sleep.type] = "falls asleep" ^^ (_ ⇒ Sleep)
  def wakeup: Parser[WakeUp.type] = "wakes up" ^^ (_ ⇒ WakeUp)

  def event: Parser[Event] = timestamp ~ (beginShift | sleep | wakeup) ^^ { case ts ~ event ⇒ Event(ts, event)}
  def events: Parser[List[Event]] = event.* <~ "\n".?
}
