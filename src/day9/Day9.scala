package day9

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {
  val fileName = "src/day9/input.txt"
  val lines = Source.fromFile(fileName).getLines().map(BigInt(_)).toVector

  //println(lines)

  def findRegression(numbers: Vector[BigInt], preamble: Int, currentIndex: Int) : (Int,BigInt) = {
    val currentNumber = numbers(currentIndex)
    val interrogatedSet = numbers.take(currentIndex+1).takeRight(preamble+1)
    val x = interrogatedSet.flatMap(a => interrogatedSet.map(b => a+b)).distinct
    //println(s" $currentNumber | $x")

    if (currentIndex + 1 == numbers.length)
      (-1,-1) //boom
    else if (x.contains(currentNumber))
      findRegression(numbers, preamble, currentIndex+1)
    else
      (currentIndex, currentNumber)
  }

  val (index, number) = findRegression(lines, 25, 25)
  println(s"Day 9 part 1 answer is ... $number")

  def findWeakness(numbers: Vector[BigInt], target: BigInt) : BigInt = {

    @tailrec
    def tryFindSum(numbers: Vector[BigInt], target: BigInt, trackMin: Int, trackMax: Int, accumulator: BigInt) : BigInt = {
      val newNumber = accumulator + numbers(trackMax)
      if (newNumber == target) {
        val subVector = numbers.slice(trackMin, trackMax)
        subVector.min + subVector.max
      } else if (newNumber > target) {
        tryFindSum(numbers, target, trackMin+1,trackMin+1,0)
      }else {
        tryFindSum(numbers, target, trackMin, trackMax+1, newNumber)
      }
    }

    tryFindSum(numbers, target,0,0, 0)
  }

  val part2answer = findWeakness(lines.take(index), number)
  println(s"Day 9 Part 2 answer is : $part2answer")
}
