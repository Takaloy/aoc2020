package day1
import scala.io.Source

object Day1 extends App {

  val fileName = "src/day1/input.txt"
  var lines  = Source.fromFile(fileName).getLines().toArray

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
