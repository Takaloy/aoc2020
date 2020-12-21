package day11

import scala.annotation.tailrec
import scala.io.Source

object Day11P2 extends App {

  val fileName = "src/day11/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  def getSeatingResolution(seating: Vector[String], position : (Int,Int)) : String = {
    val (x,y) = position
    val seat = seating(x)(y)

    seat match {
      case '.' => "."
      case 'L' if !getAdjacentSeats(seating, position).contains('#') => "#"
      case '#' if getAdjacentSeats(seating,position).count(_ == '#') >= 5 => "L"
      case _ => seat.toString
    }
  }

  def getAdjacentSeats(seating: Vector[String], position: (Int,Int)) : Vector[Char] = {
    val (x,y) = position

    @tailrec
    def getRelevantSeat(seating: Vector[String], trajectory: Vector[(Int,Int,Int,Int)], accumulator: Vector[Char]) : Vector[Char] = {
      if (trajectory.isEmpty)
        accumulator
      else {
        val (x,y,up,right) = trajectory.head
        if (x >= 0 && y >= 0 && x < seating.length && y < seating(0).length) {
          val newPosition = seating(x)(y)
          if (newPosition != '.')
            getRelevantSeat(seating, trajectory.tail, accumulator :+ newPosition)
          else
            getRelevantSeat(seating, (x+up,y+right,up,right) +: trajectory.tail, accumulator)
        } else {
          getRelevantSeat(seating, trajectory.tail, accumulator)
        }
      }
    }

    val adjacencyList = Vector(
      (x-1,y-1,-1,-1), (x,y-1,0,-1), (x+1,y-1,+1,-1),
      (x-1,y,-1,0), (x+1,y,+1,0),
      (x-1,y+1,-1,+1), (x,y+1,0,+1), (x+1,y+1,+1,+1))

    getRelevantSeat(seating, adjacencyList, Vector())

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
  println(s"Day 11 part 2 answer: there are ${occupiedSeats} occupied.")
}
