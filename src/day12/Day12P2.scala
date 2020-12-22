package day12

import scala.io.Source

object Day12P2 extends App {

  val fileName = "src/day12/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  val wayPoint = WayPoint(4,10)
  val ship = ShipV2(0,0,WayPoint(1,10))

  println(ship.move(lines))
  println("Day 12 Part 2 answer is " + ship.move(lines).getManhattanDistance())
}

case class ShipV2(north: Int, east: Int, wayPoint: WayPoint) {
  private def forwardMove(number: Int) : ShipV2 = {
    ShipV2(this.north + wayPoint.north * number, this.east + wayPoint.east * number, wayPoint)
  }

  def move(input: String) : ShipV2 = {
    val output = input.split("",2)
    val (direction, distance) = ((output(0)(0)),output(1).toInt)

    direction match {
      case 'F' => forwardMove(distance)
      case _ => ShipV2(north,east, wayPoint.move(direction, distance))
    }
  }

  def move(ship: ShipV2, input: Vector[String]) : ShipV2 = {
    if (input.isEmpty)
      ship
    else
      move(ship.move(input.head), input.tail)
  }

  def move(input: Vector[String]) : ShipV2 = {
    move(this, input)
  }

  def getManhattanDistance() : Int = {
    east.abs + north.abs
  }
}

case class WayPoint(north: Int, east: Int) {

  //I am lazy but you can solve this using trigonometry
  def pivot(direction: Char, angle: Int): WayPoint = {
    direction match {
      case 'R' if angle == 90 => WayPoint(-east, north)
      case 'R' if angle == 270 => WayPoint(east,-north)
      case 'L' if angle == 90 => WayPoint(east, -north)
      case 'L' if angle == 270 => WayPoint(-east, north)
      case _ if angle == 180 => WayPoint(-north, -east)
      case _ => {
        println("oops?")
        this
      }
    }
  }

  def move(direction: Char, distance: Int) : WayPoint = {

    direction match {
      case 'N' => WayPoint(north+distance, east)
      case 'S' => WayPoint(north-distance, east)
      case 'E' => WayPoint(north, east+distance)
      case 'W' => WayPoint(north, east-distance)
      case x if (List('R','L').contains(x)) => pivot(direction, distance)
      case _ => throw new Exception(s"${direction}")
    }
  }
}