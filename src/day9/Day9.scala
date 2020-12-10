package day9

import scala.io.Source

object Day9 extends App {
  val fileName = "src/day9/input.txt"
  val lines = Source.fromFile(fileName).getLines().map(BigInt(_)).toVector

  //println(lines)

  def findRegression(numbers: Vector[BigInt], preamble: Int, currentIndex: Int) : BigInt = {
    val currentNumber = numbers(currentIndex)
    val interrogatedSet = numbers.take(currentIndex+1).takeRight(preamble+1)
    val x = interrogatedSet.flatMap(a => interrogatedSet.map(b => a+b)).distinct
    //println(s" $currentNumber | $x")

    if (currentIndex + 1 == numbers.length)
      -1  //boom
    else if (x.contains(currentNumber))
      findRegression(numbers, preamble, currentIndex+1)
    else
      currentNumber
  }

  val number = findRegression(lines, 25, 25)
  println(s"Day 9 part 1 answer is ... $number")
}
