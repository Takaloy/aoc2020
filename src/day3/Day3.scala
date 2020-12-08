package day3

import scala.io.Source

object Day3 extends App{

  val fileName = "src/day3/input.txt"
  val lines  = Source.fromFile(fileName).getLines().toVector

  //right first then down
  def trees(lines: Vector[String], start: Int, right: Int, down: Int, total: Int) : Int = {

    val remainingLines = lines.drop(down)
    val row = remainingLines.head

    val segment = getSegment(row, start, right)

    val newTotal = if (segment.last == '#') total + 1 else total
    val newStart = (start + right) % row.length

    //println(f"$row : $newStart from $start , $newTotal, ${row.length} starting with ${segment.last} on $segment")

    if (remainingLines.length <= down)
      newTotal
    else
      trees(remainingLines, newStart, right, down, newTotal)
  }

  def getSegment(row: String, start: Int, right: Int) : String = {
    val shiftRight = 1
    val overflow = start + right - row.length + shiftRight //defines if we need to get more stuff

    if (overflow > 0)
      row.substring(start) + row.substring(0,overflow)
    else
      row.substring(start, start + right + shiftRight)
  }

  val numberOfTrees = trees(lines,0,3,1,0)
  println(numberOfTrees)

  val part2Trees =
    trees(lines,0,1,1,0) *
    trees(lines,0,3,1,0) *
    trees(lines,0,5,1,0) *
    trees(lines,0,7,1,0) *
      trees(lines,0,1,2,0)

  println(part2Trees)
}
