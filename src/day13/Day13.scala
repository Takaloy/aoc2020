package day13

import scala.io.Source

object Day13 extends App {

  val fileName = "src/day13/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  val (timeStamp, bussesId) = (BigInt(lines(0)),lines(1).split(",").filter(_ != "x").map(_.toInt).toVector)

  println(timeStamp)
  println(bussesId)

  val departFromNow = bussesId.map(a => timeStamp - timeStamp % a + a - timeStamp)
  val (fastestDepartFromNow, fastestDepartId) = (departFromNow.min, bussesId(departFromNow.indexOf(departFromNow.min)))

  println(s"Day 13 part 1 answer is ${fastestDepartFromNow * fastestDepartId}")
}
