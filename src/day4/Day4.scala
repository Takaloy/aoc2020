package day4

import scala.::
import scala.io.Source

object Day4 extends App {

  val fileName = "src/day4/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  def getRecords(lines: Vector[String], scratchpad: String, accumulator: Vector[String]): Vector[String] = {
    if (lines.length == 0)
      accumulator :+ scratchpad
    else if (lines.head.isEmpty) getRecords(lines.drop(1), null, accumulator :+ scratchpad)
    else getRecords(lines.drop(1), if (scratchpad == null) lines.head else scratchpad + " " + lines.head, accumulator)
  }

  val test = getRecords(lines,null,Vector[String]())

  def isValid(items: String) : Boolean = {
    val kv = for {
      (k,v) <- items.split(" ").map(_.split(":")).map(a => a(0) -> a(1)).toMap
    } yield (k,v)

    val requiredFields = List("byr","iyr","eyr","hgt","hcl","ecl","pid")

    requiredFields.forall(r => kv.contains(r))
  }

  val validPassports = test count isValid
  println(f"valid passports $validPassports of ${test.length}")
}
