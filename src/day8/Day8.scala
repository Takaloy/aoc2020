package day8

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val fileName = "src/day8/input.txt"
  val lines = Source.fromFile(fileName).getLines().toVector

  val instructions =
    (1 to lines.size).zip(
      lines.map(l => {
        val r = l.split(" ")
        (r(0),r(1).toInt)
      })).toVector

  //instructions.foreach(println)

  def runInstructions(instructions:  Vector[(Int, (String, Int))]) : Int = {
    @tailrec
    def runSubRoutine(instructions:  Vector[(Int, (String, Int))], executedRoutine : Set[Int], currentRun: Int, accumulator : Int) : Int = {

      if (instructions.length == currentRun)
        accumulator
      else if (executedRoutine.contains(currentRun))
        accumulator
      else {
        val (action,value) = instructions(currentRun)._2
        if (action == "acc")
          runSubRoutine(instructions, executedRoutine + currentRun, currentRun+1, accumulator + value)
        else if (action == "jmp")
          runSubRoutine(instructions, executedRoutine + currentRun, currentRun+value, accumulator)
        else
          runSubRoutine(instructions, executedRoutine + currentRun, currentRun+1, accumulator)
      }
    }
    runSubRoutine(instructions,Set(),0,0)
  }

  val answer = runInstructions(instructions)
  println(f"Answer for Day 8 Part 1 is $answer.")

  //part 2

  def findSwappedInstructions(instructions:  Vector[(Int, (String, Int))]) : (Int, Int) = {

    @tailrec
    def executeSubRoutine(instructions:  Vector[(Int, (String, Int))], executedRoutine : Set[Int], currentRun: Int = 0) : Set[Int] = {
      if (instructions.length == currentRun)
        executedRoutine + currentRun
      else if (executedRoutine.contains(currentRun))
        executedRoutine
      else {
        val (action,value) = instructions(currentRun)._2
        if (action == "acc")
          executeSubRoutine(instructions, executedRoutine + currentRun, currentRun+1)
        else if (action == "jmp")
          executeSubRoutine(instructions, executedRoutine + currentRun, currentRun+value)
        else
          executeSubRoutine(instructions, executedRoutine + currentRun, currentRun+1)
      }
    }

    val executedRoutine = executeSubRoutine(instructions, Set())

    val possibleCombinations =
      executedRoutine.filter(nop => instructions(nop)._2._1 == "nop")
        .flatMap(a => executedRoutine
          .filter(jmp => instructions(jmp)._2._1 == "jmp")
          .map(b => (a,b)))

    println(possibleCombinations)

    @tailrec
    def pushSubRoutine(instructions:  Vector[(Int, (String, Int))], possibleCombo: Set[(Int,Int)]) : (Int,Int) = {
      if (possibleCombo.size == 0)
        (-1, -1)
      else {
        val inv = possibleCombo.head
        val modifiedInstructions = instructions
          .updated(inv._1, (instructions(inv._1)._1, swapInstructions(instructions(inv._1)._2)))
          .updated(inv._2, (instructions(inv._2)._1, swapInstructions(instructions(inv._2)._2)))

        val successfulRoutine = executeSubRoutine(modifiedInstructions, Set())
        if (successfulRoutine.contains(instructions.length))  //last element executed
          inv
        else
          pushSubRoutine(instructions, possibleCombo.drop(1))
      }
    }

    pushSubRoutine(instructions, possibleCombinations)
  }

  def swapInstructions(x : (String,Int)): (String, Int) = {
    val (action, value) = x
    if (action == "jmp") ("nop", value)
    else if (action == "nop") ("jmp",value)
    else (action,value)
  }

  val swapped = findSwappedInstructions(instructions)
  println(swapped)

  val newInstructions = instructions
    .updated(swapped._1, (instructions(swapped._1)._1, swapInstructions(instructions(swapped._1)._2)))
    .updated(swapped._2, (instructions(swapped._2)._1, swapInstructions(instructions(swapped._2)._2)))

  val part2answer = runInstructions(newInstructions)
  println(s"Answer to Day 8 Part 2 : $part2answer")
}
