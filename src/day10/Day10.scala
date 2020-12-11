package day10

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val fileName = "src/day10/input.txt"
  val lines = Source.fromFile(fileName).getLines().map(_.toInt).toVector.sorted
  val jolts = 0 +: lines :+ lines.last + 3

  def getJoltDifferences(jolts: Vector[Int]) : Set[(Int,Int)] = {
    @tailrec
    def subRouteCalc(jolts: Vector[Int], accumulator: List[Int]) : List[Int] = {
      if (jolts.length == 1)
        accumulator
      else {
        val difference = jolts(1) - jolts(0)
        subRouteCalc(jolts.drop(1), difference :: accumulator)
      }
    }
    val allDiff = subRouteCalc(jolts,List())
    allDiff.groupBy(l => l).map(a => (a._1, a._2.size)).toSet
  }

  val setGrouped = getJoltDifferences(jolts)
  println(setGrouped)
  val answer = setGrouped.find(l => l._1 == 1).head._2 * setGrouped.find(l => l._1 == 3).head._2
  println(s"Day 10 Part 1 answer is : $answer" )
}
