package day2

import scala.io.Source

object Day2 extends App {

  val fileName = "src/day2/input.txt"
  val lines  = Source.fromFile(fileName).getLines().toArray

  case class PasswordRecords(min: Int, max: Int, key: Char, password: String)

  val records = lines.map({
    l =>
      val s = l.split(" ")
      val minmax = s(0).split("-")
      PasswordRecords(minmax(0).toInt,minmax(1).toInt,s(1).charAt(0),s(2))
  })

  for {
    r <- records
  }println(r.min + "-" + r.max + " " + r.key + " " + r.password + " matched : " + r.password.count(_ == r.key))

  val valid = records.filter(r => {
    val matched = r.password.count(_ == r.key)
    if (matched >= r.min && matched <= r.max) true else false
  })

  println("[PART 1] valid password records: " + valid.length + " of a total of " + records.length)


  //part 2: https://adventofcode.com/2020/day/2#part2

  val validAtPosition = records.filter(r =>
    (r.password.charAt(r.min-1) == r.key || r.password.charAt(r.max-1) == r.key)
      && !(r.password.charAt(r.min-1) == r.key && r.password.charAt(r.max-1) == r.key))


  println("[PART 2] valid password records: " + validAtPosition.length + " of a total of " + records.length)
}
