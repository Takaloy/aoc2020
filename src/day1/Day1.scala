package day1
import scala.io.Source

/**
 * This solution is quick and dirty to get to the right answer but it is the wrong solution.
 * When you pick any two numbers (or three), you might come into a situation where it picks itself.
 * in this scenario 2020, if any of the input is 1010, then the current solution will present a wrong result,
 * as the request is to use any TWO numbers not pick itself twice.
 *
 * test for wrongness :
 * part 1
 *  t = (a*2)
 * part 2
 *  t = (a*2) + b
 *
 * proposed solution when I have time ...
 * convert lines to linked list. which looks something like this ...
 *
 * def isCorrectCombo(a, b) {
 *  if (b is Nothing)
 *    return false
 *  else if (a + b == 2020)
 *    return true
 *  else
 *    return isCorrectCombo(a, b.tail)
 * }
 *
 * val answer = for {
 *  number <- numbers if isCorrectCombo(number,number.tail)
 * } yield number * number.tail
 */
object Day1 extends App {

  val fileName = "src/day1/input.txt"
  val lines  = Source.fromFile(fileName).getLines().toArray

  //part 1
  val combinations = lines.flatMap(a => lines.map(b => (a.toInt,b.toInt)))

  val answer = for {
    (a,b) <- combinations if a + b == 2020
  } yield a * b

  for {
    n <- answer
  } println("part 1" + n)

  //part 2
  val tripleCombo = lines.flatMap(a => lines.flatMap(b => lines.map(c => (a.toInt,b.toInt,c.toInt))))

  val part2answer = for {
    (a,b,c) <- tripleCombo if (a + b + c) == 2020
  } yield a * b * c

  for {
    n <- part2answer
  } println("part 2: " + n)
}
