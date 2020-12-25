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

//  def getStep1(positionAndIds : Vector[(Int,Int)], index: Int = 0, accumulator: String = "") : String = {
//    if (positionAndIds.length == index)
//      accumulator
//    else {
//      val step1 = positionAndIds
//        .filter(_ != positionAndIds(index))
//        .map(a => BigInt(a._2))
//        .reduce(_ * _)
//
//      val mod = positionAndIds(index)._2
//      val desiredRemainder = positionAndIds(index)._1
//
//      //val step2 : (BigInt, Int, Int) = (step1,(step1 % mod).toInt, bruteForceFindRightMultiplier((step1 % mod).toInt,desiredRemainder,mod))
//
//      val step2 = (step1, (step1 % mod))
//      getStep1(positionAndIds, index+1, accumulator + " + " + step2)
//    }
//  }
//
//  def getPart2Result(positionAndIds : Vector[(Int,Int)], index: Int = 0, accumulator: BigInt = 0) : BigInt = {
//    if (positionAndIds.length == index)
//      accumulator
//    else {
//      val step1 = positionAndIds
//        .filter(_ != positionAndIds(index))
//        .map(a => BigInt(a._2))
//        .reduce(_ * _)
//
//      val mod = positionAndIds(index)._2
//      val desiredRemainder = (mod - positionAndIds(index)._1) % mod
//
//      val step2 = step1 * bruteForceFindRightMultiplier((step1 % mod).toInt, desiredRemainder, mod)
//
//      getPart2Result(positionAndIds, index+1, accumulator + step2)
//    }
//  }
//
//  @tailrec
//  def bruteForceFindRightMultiplier(currentRemainder: Int, desiredRemainder: Int, modulo: Int, index: Int = 1): Int = {
//    if ((currentRemainder * index) % modulo == desiredRemainder)
//      index
//    else if (desiredRemainder > modulo) {
//      //bruteForceFindRightMultiplier(currentRemainder, desiredRemainder, modulo * (desiredRemainder/modulo + 1), index)
//      bruteForceFindRightMultiplier(currentRemainder, desiredRemainder % modulo, modulo, index)
//    }
//    else {
//      bruteForceFindRightMultiplier(currentRemainder,desiredRemainder,modulo,index+1)
//    }
//  }
//
//  //println(getStep1(positionAndIds))
//  //println(getPart2Result(positionAndIds))
//
//  //chineseRemainder(List(41,37,379,23,13,17,29,557,19),List(0,35,41,49,54,58,70,72,91))
//  //val result = chineseRemainder(List(41,37,379,23,13,17,29,557,19),List(0,35,41,49,54,58,70,72,91))


  val remainders = chineseRemainder(positionAndIds.map(a => BigInt(a._2)).toList,
    positionAndIds.map(a => BigInt(a._2 - (if (a._1 != 0) a._1 % a._2 else a._2))).toList)
  println(remainders)


}
