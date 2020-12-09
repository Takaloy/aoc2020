package day5

import scala.io.Source

object Day5 extends App {

  val fileName = "src/day5/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector


  def decodeBinarySeating(value : String, minValue: Int, maxValue: Int, indicateLowerThreshold : Char) : Int = {
    val midPoint = (maxValue - minValue + 1) / 2
    val keepFront = value.charAt(0) == indicateLowerThreshold

    val newMax = if (keepFront) maxValue - midPoint else maxValue
    val newMin = if (keepFront) minValue else minValue + midPoint

    if (newMax == newMin || value.length == 1)
      newMin
    else
      decodeBinarySeating(value.drop(1),newMin,newMax,indicateLowerThreshold)
  }

  val testValue = "FBFBBFFRLR"
  val row = decodeBinarySeating(testValue.take(7),0,127,'F')
  val column = decodeBinarySeating(testValue.substring(7),0,7,'L')
  println(f"row $row , column $column with seat id ${getSeatId(row,column)}" )

  def getSeatId(row: Int, column: Int) : Int = {
    row * 8 + column
  }

  def getSeatId(line : String) : Int = {
    val row = decodeBinarySeating(line.take(7),0,127,'F')
    val column = decodeBinarySeating(line.substring(7),0,7,'L')

    getSeatId(row,column)
  }

  val highestSeatId = lines.map(getSeatId).max

  println(f"day 5 part 1 answer is .. $highestSeatId")

  val allSeats = 85 to highestSeatId
  val usedSeats = lines.map(getSeatId)

  val emptySeats = allSeats.filter(a => !usedSeats.contains(a))

  val result = emptySeats.mkString(",")
  println(f"day 5 part 2 answer is ... ${result}")
}
