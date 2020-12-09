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

    requiredFields.forall(r => kv.contains(r)) && kv.forall(k => isValidData(k._1,k._2))
  }

  def isValidData(k : String, v : String) : Boolean = (k,v) match {
    case ("byr", v) => v.length == 4 && v.toInt >= 1920 && v.toInt <= 2002
    case ("iyr", v) => v.length == 4 && v.toInt >= 2010 && v.toInt <= 2020
    case ("eyr", v) => v.length == 4 && v.toInt >= 2020 && v.toInt <= 2030
    case ("hgt", v) => hgtMatch(v)
    case ("hcl", v) => v matches "^#[a-fA-F0-9]{6}$"
    case ("ecl", v) => List("amb","blu","brn","gry","grn","hzl","oth").contains(v)
    case ("pid", v) => v matches "^[0-9]{9}$"
    case _ => true
  }

  def hgtMatch(v: String) : Boolean = v match {
    case v if v.endsWith("cm") => v.substring(0,v.length-2).toInt >= 150 && v.substring(0,v.length-2).toInt <=193
    case v if v.endsWith("in") => v.substring(0,v.length-2).toInt >= 59 && v.substring(0,v.length-2).toInt <=76
    case _ => false
  }

  val validPassports = test count isValid
  println(f"valid passports $validPassports of ${test.length}")

}
