package day13

import day13.ChineseRemainderTheorem.chineseRemainder

import scala.annotation.tailrec
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


  //part 2: chinese remainder theorem easier to do on paper than code ... ???

  val busses = lines(1).split(",");
  val positionAndIds = Array.tabulate(busses.length) {i => (i, busses(i))}
    .filter(x => x._2 != "x")
    .map(a => (a._1,a._2.toInt))
    .toVector

  println(positionAndIds)

  val remainders = chineseRemainder(positionAndIds.map(a => BigInt(a._2)).toList,
    positionAndIds.map(a => BigInt(a._2 - (if (a._1 != 0) a._1 % a._2 else a._2))).toList)
  println(remainders)


}
