package day7

import scala.io.Source
import scala.annotation.tailrec

object Day7 extends App {

  val fileName = "src/day7/input.txt"
  val lines = Source.fromFile(fileName).getLines().toList

  val records = lines.map(l => {
    val r = l.split("contain")
    (removeExcess(r(0)),
      removeExcess(r.drop(1).mkString(""))
        .split(",")
        .filter(_ != "no other")
        .map(a => {
          val b = a.trim.split(" ")
          (b.drop(1).mkString(" "), b(0).toInt)
        }).toSet
    )}).toMap

  def removeExcess(input: String) : String = {
    val regex = "(\\bbag[s]{0,1}[.]{0,1})".r
    regex.replaceAllIn(input,"").trim
  }

  //test records are populated correctly
  //records.foreach(r => println(s"${r._1} | ${r._2.mkString(" ")}"))

  def canFitIntoBags(policy: Map[String, Set[(String, Int)]], target: String, subject: String) : Boolean = {

    @tailrec
    def canFitIn(target: String, consideredBag: Set[(String, Int)], discoveredBag: Set[(String, Int)]) : Boolean = {
      if (discoveredBag.isEmpty)
        false
      else {
        val bag = discoveredBag.head
        if (bag._1 == target)
          true
        else if (consideredBag.exists(c => c._1 == bag._1))
          canFitIn(target,consideredBag,discoveredBag.tail)
        else
          canFitIn(target, consideredBag + bag, discoveredBag.tail ++ policy(bag._1))
      }
    }
    canFitIn(target,Set(), policy(subject))
  }

  def haveNumberOfOptionsFor(policy: Map[String, Set[(String, Int)]], target: String) : Int = {
    policy.count(p => canFitIntoBags(policy, target, p._1))
  }

  println(s"Day 7 part 1 answer: ${haveNumberOfOptionsFor(records,"shiny gold")}")

  def bagsRequiredFor(policy: Map[String, Set[(String, Int)]], target: String, accumulator: Int = 0) : Int = {
    val targetPolicy = policy(target)

    if (targetPolicy.isEmpty)
      accumulator
    else {
      targetPolicy
        .foldLeft(0) {
          case (accB, (a, b)) => (accB + (bagsRequiredFor(policy,a) * b + b))
        }
    }
  }

  println(f"Day 7 part 2 answer: ${bagsRequiredFor(records,"shiny gold")}")
}
