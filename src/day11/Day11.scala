package day11

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  val fileName = "src/day11/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  def getSeatingResolution(seating: Vector[String], position : (Int,Int)) : String = {
    val (x,y) = position
    val seat = seating(x)(y)

    seat match {
      case '.' => "."
      case 'L' if !getAdjacentSeats(seating, position).contains('#') => "#"
      case '#' if getAdjacentSeats(seating,position).count(_ == '#') >= 4 => "L"
      case _ => seat.toString
    }
  }

  def getAdjacentSeats(seating: Vector[String], position: (Int,Int)) : Vector[Char] = {
    val (x,y) = position

    Vector((x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y),(x-1,y+1), (x,y+1), (x+1,y+1))
      .filter(v => v._1 >= 0 && v._2 >= 0 && v._1 < seating.length && v._2 < seating(0).length)
      .map(m => seating(m._1)(m._2))
      .filter(n => n != '.')

  }

  def occupySeating(seating: Vector[String]) : Vector[String] = {
    (for {
      r <- 0 to seating.length-1
      c <- 0 to seating(0).length-1
    } yield (getSeatingResolution(seating,(r,c)))
      ).grouped(seating(0).length).toVector.map(a => a.reduce((x,y) => x+y))
  }


  @tailrec
  def getFinalSeatingPlan(seating: Vector[String]) : Vector[String] = {
    val newSeating = occupySeating(seating)

    if (newSeating sameElements( seating))
      newSeating
    else
      getFinalSeatingPlan(newSeating)
  }


  val result = getFinalSeatingPlan(lines)
  result.foreach(println)

  val occupiedSeats = result.mkString("").count(_ == '#')
  println(s"Day 11 part 1 answer: there are ${occupiedSeats} occupied.")
}
