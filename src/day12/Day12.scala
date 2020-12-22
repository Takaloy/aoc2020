package day12

import org.graalvm.compiler.graph.Node.Input

import scala.math.abs
import scala.io.Source

object Day12 extends App {

  val fileName = "src/day12/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  val ship = Ship('E',0,0).move(lines)

  println("Day 12 part 1 answer is : " + ship.getManhattanDistance())

}

case class Ship(cardinalDirection: Char, north: Int, east: Int) {

  def move(input: Vector[String]) : Ship = {
    move(this, input)
  }

  def move(ship: Ship, input: Vector[String]) : Ship = {
    if (input.isEmpty)
      ship
    else {
      val newShip = ship.move(input.head)
      println(newShip + "\t" + " | " + input.head )
      move(newShip, input.tail)
    }
  }

  def move(input: String) : Ship = {
    val output = input.split("",2)
    val (direction, distance) = ((output(0)(0)),output(1).toInt)

    move(direction,distance)
  }



  def pivot(direction: Char, angle: Int) : Ship = {
    val newCardinalDirection = direction match {
      case 'L' => getCardinalDirection(getCardinalValues(this.cardinalDirection) - angle)
      case 'R' => getCardinalDirection(getCardinalValues(this.cardinalDirection) + angle)
      case 'F' => this.cardinalDirection
      case _ => direction
    }

    Ship(newCardinalDirection, this.north, this.east)
  }

  def move(direction: Char, distance: Int) : Ship = {

    direction match {
      case x if (List('R','L').contains(x)) => pivot(direction, distance)
      case 'F' => move(this.cardinalDirection, distance)
      case 'N' => Ship(this.cardinalDirection,north+distance,east)
      case 'S' => Ship(this.cardinalDirection,north-distance,east)
      case 'E' => Ship(this.cardinalDirection,north, east+distance)
      case 'W' => Ship(this.cardinalDirection,north,east-distance)
      case _ => throw new Exception(s"${direction}")
    }
  }

  def getManhattanDistance() : Int = {
    east.abs + north.abs
  }

  private def getCardinalValues(direction: Char) : Int = {
    direction match {
      case 'N' => 0
      case 'E' => 90
      case 'S' => 180
      case 'W' => 270
      case _ => throw new IllegalArgumentException
    }
  }

  private def getCardinalDirection(cardinal: Int) : Char = {
    cardinal match {
      case x if x >= 0 && x < 90 => 'N'
      case x if x >= 90 && x < 180 => 'E'
      case x if x >=180 && x < 270 => 'S'
      case x if x >=270 && x < 360 => 'W'
      case x if x < 0 => getCardinalDirection((360 + x) % 360)
      case _ => getCardinalDirection(cardinal % 360)

    }
  }
}