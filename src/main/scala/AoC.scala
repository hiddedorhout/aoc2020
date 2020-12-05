import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object AoC extends App {

  val day1FileName = "src/main/scala/account.txt"
  val day2FileName = "src/main/scala/passwords.txt"
  val day3FileName = "src/main/scala/trees.txt"
  val day4FileName = "src/main/scala/passports.txt"
  val day5FileName = "src/main/scala/plane.txt"
  val bufferedSource = Source.fromFile(day5FileName)

  val input = bufferedSource.getLines().toList

  // Day 1
  @scala.annotation.tailrec
  def add2To2020(l: List[Int]): Int = {
    l.tail.map(i => (i + l.head, i)).find(_._1 == 2020) match {
      case Some(value) => l.head * value._2
      case None        => add2To2020(l.tail)
    }
  }

  @scala.annotation.tailrec
  def add3To2020(l: List[Int]): Int = {
    l.tail
      .flatMap(second => {
        l.tail.tail map (third => {
          (l.head + second + third, (second, third))
        })
      })
      .find(_._1 == 2020) match {
      case Some(value) => value._2._1 * value._2._2 * l.head
      case None        => add3To2020(l.tail)
    }
  }

  //day2

  case class PolicyAndPassword(character: Char,
                               min: Int,
                               max: Int,
                               password: String)

  def parseInput(in: List[String]): List[PolicyAndPassword] = {
    in map (raw => {
      val (rawPolicy, rawPassword) =
        raw.replaceAll("\\s", "").splitAt(raw.indexOf(":"))
      val character = rawPolicy.replace(":", "").toList.reverse.head
      val (rawMin, rawMax) = rawPolicy.splitAt(rawPolicy.indexOf("-"))
      val max = ("""\d""".r findAllIn rawMax).toList.mkString("").toInt
      PolicyAndPassword(character, rawMin.toInt, max, rawPassword)
    })
  }

  def countInstances(c: Char, in: String): Int = in.count(_ == c)

  def validPasswordCount1(in: List[PolicyAndPassword]): Int =
    in.count(i => {
      i.min <= countInstances(i.character, i.password) && countInstances(
        i.character,
        i.password
      ) <= i.max
    })

  def validPasswordCount2(in: List[PolicyAndPassword]): Int =
    in.count(i => {
      (i.password(i.min - 1) == i.character && i
        .password(i.max - 1) != i.character) ||
      (i.password(i.min - 1) != i.character && i
        .password(i.max - 1) == i.character)
    })

  //day 3

  trait Slope
  case object Open extends Slope
  case object Tree extends Slope

  val treeInput =
    input.map(line => line.toList.map(c => if (c == '.') Open else Tree))

  def elongate(original: List[Slope], howOften: Int): List[Slope] =
    List.fill(howOften)(original).flatten

  def treesToHit(mountain: List[List[Slope]])(right: Int, down: Int) = {
    if (down == 1) {
      mountain.reverse
        .foldRight((List[List[Slope]](), 0))((line, b) => {
          val (route, whereWeAre) = b
          val startingPoint = whereWeAre * right
          val inLine = elongate(
            line,
            if (startingPoint + right > line.length) whereWeAre
            else 1
          )
          val appending = inLine.slice(startingPoint, startingPoint + right)
          val nextRout = appending :: route
          (nextRout, whereWeAre + down)
        })
        ._1
        .reverse
        .map(part => {
          part.head
        })
        .count(_ == Tree)
    } else {
      mountain.reverse
        .grouped(down)
        .map(l => l.head)
        .foldRight((List[List[Slope]](), 0))((line, b) => {
          val (route, whereWeAre) = b
          val startingPoint = if (whereWeAre == 0) 0 else whereWeAre / 2
          val inLine = elongate(
            line,
            if (startingPoint + right > line.length) whereWeAre
            else 1
          )
          val appending = inLine.slice(startingPoint, startingPoint + right + 1)
          val nextRout = appending :: route
          (nextRout, whereWeAre + down)
        })
        ._1
        .reverse
        .map(part => {
          part.head
        })
        .count(_ == Tree)
    }
  }

//  val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
//  val res = slopes.map(i => treesToHit(treeInput)(i._1, i._2))
//  println(res)
//  println(res.foldLeft(1L)(_ * _.toLong))

  //day 4

  def cleanInput(in: List[String]) =
    in.map(l => if (l == "") "|" else l)
      .mkString(" ")
      .split("\\|")
      .toList

  def parseThatInput(in: List[String]) =
    in.map(l => {
      val map: Map[String, String] = l.trim
        .split(" ")
        .map(kvPair => {
          val kv = kvPair.split(":")
          (kv(0), kv(1))
        })
        .toMap
      map
    })

  def isValid(in: Map[String, String]): Boolean = {
    if (in.keys.toList.length == 8 || (in.keys.toList.length == 7 && !in
          .contains("cid"))) {
      validByr(in("byr")) && validIyr(in("iyr")) && validEyr(in("eyr")) && validHgt(
        in("hgt")
      ) && validHcl(in("hcl")) && validEcl(in("ecl")) && validPid(in("pid"))
    } else false
  }

  def validByr(in: String): Boolean =
    in.length == 4 && 2002 >= in.toInt && in.toInt >= 1920
  def validIyr(in: String): Boolean =
    in.length == 4 && 2020 >= in.toInt && in.toInt >= 2010
  def validEyr(in: String): Boolean =
    in.length == 4 && 2030 >= in.toInt && in.toInt >= 2020
  def validHgt(in: String): Boolean = {
    val patternValue = "\\d*".r
    val patternUnit = "\\D{2}".r
    (patternUnit findFirstIn in, patternValue findFirstIn in) match {
      case (Some("cm"), Some(value)) => value.toInt >= 150 && value.toInt <= 193
      case (Some("in"), Some(value)) => value.toInt >= 59 && value.toInt <= 76
      case _                         => false
    }
  }
  def validHcl(in: String): Boolean = {
    val pattern = "^#[a-f0-9]{6}".r
    pattern.matches(in)
  }
  def validEcl(in: String): Boolean =
    List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(in)
  def validPid(in: String): Boolean = in.length == 9

//  println(cleanInput(input).length)
//  println(parseThatInput(cleanInput(input)))
//  println(parseThatInput(cleanInput(input)).map(isValid).count(_ == true))

  // day 5
  val rowsInPlane = List.range(0, 128) // 2^7
  val columnsInPLane = List.range(0, 8)

  val allSeatIds = columnsInPLane.flatMap(column => {
    rowsInPlane.map(row => {
      (row * 8) + column
    })
  })

  case class Seat(row: Int, column: Int) {
    def id: Int = (this.row * 8) + this.column
  }

  def getSeat(in: String): Seat = {
    val row = in
      .slice(0, 7)
      .foldLeft(List[List[Int]](rowsInPlane))((a, b) => {
        val (l, r) = a.head.splitAt(a.head.length / 2)
        b match {
          case 'F' => {
            l :: a
          }
          case 'B' => {
            r :: a
          }
        }
      })
      .head
      .head
    val column = in
      .slice(7, in.length + 1)
      .foldLeft(List[List[Int]](columnsInPLane))((a, b) => {
        val (l, r) = a.head.splitAt(a.head.length / 2)
        b match {
          case 'L' => {
            l :: a
          }
          case 'R' => {
            r :: a
          }
        }
      })
      .head
      .head
    Seat(row, column)
  }

  val knownTickets = input.map(ticket => getSeat(ticket).id)
  println(allSeatIds.length)

  println(allSeatIds.filterNot(a => knownTickets.contains(a)).sorted)

  bufferedSource.close()
}
