package day6

import scala.io.Source

object Day6 extends App {
  val fileName = "src/day6/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  def getRecords(lines: Vector[String], scratchpad: String, accumulator: Vector[String]): Vector[String] = {
    if (lines.length == 0)
      accumulator :+ scratchpad
    else if (lines.head.isEmpty) getRecords(lines.drop(1), null, accumulator :+ scratchpad)
    else getRecords(lines.drop(1), if (scratchpad == null) lines.head else scratchpad + " "+ lines.head, accumulator)
  }

  val test = getRecords(lines,null,Vector[String]())

  def sumOfAllYes(input: Vector[String], accumulator : Int): Int = {
    if (input.length == 0)
      accumulator
    else
      sumOfAllYes(input.drop(1),accumulator + input.head.filter(_ != ' ').toSeq.distinct.length)
  }

  val totalYeses = sumOfAllYes(test,0)
  println("Day 6 part 1 answer is " + totalYeses)

  def commonYeses(line: String) : Int = {
    line.toSeq.distinct.count(d => line.split(" ").forall(r => r.contains(d)))
  }

  def sumOfCommonYeses(input: Vector[String], accumulator: Int) : Int = {
    if (input.length == 0)
      accumulator
    else
      sumOfCommonYeses(input.drop(1),accumulator + commonYeses(input.head))
  }

  println("Day 6 part 2 answer is " + sumOfCommonYeses(test,0))
}
