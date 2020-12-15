package day10

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val fileName = "src/day10/input.txt"
  val lines = Source.fromFile(fileName).getLines().map(_.toInt).toVector.sorted
  val jolts = 0 +: lines :+ lines.last + 3
  println(jolts)

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

  def calculatePossibleRoutes(jolts: Vector[Int]): Vector[BigInt] = {

    @tailrec
    def routeCalcSubRoutine(jolts: Vector[Int], counter : Int, accumulator: Vector[BigInt]) : Vector[BigInt] = {
      if (counter == jolts.length-1)
        accumulator
      else if (counter == 0)
        routeCalcSubRoutine(jolts, counter + 1, accumulator :+ 1)
      else {
        val acceptJolt = Vector(1,2,3)
        val paths = acceptJolt.map(jolts(counter) - _)
          .filter(x => x >= 0 && jolts.exists(j => j == x))
          .map(b => accumulator(jolts.indexOf(b)))
          .sum

        println(s"sum of all paths for $counter is $paths")
        routeCalcSubRoutine(jolts, counter + 1, accumulator :+ paths)
      }
    }

    routeCalcSubRoutine(jolts, 0, Vector())
  }

  println(s"Day 10 part 2 answer is ${calculatePossibleRoutes(jolts).last}")
}
