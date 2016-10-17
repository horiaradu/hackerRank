package sherlockAndCost

import scala.math.abs

object SherlockAndCost {
  def iterate(current: Array[(Int, Int)], b: Int): Array[(Int, Int)] = {
  	Array(1, b) map ( choice =>
  		(current map {
  			case (element, sum) => (choice, sum + abs(element - choice))
  		}).max(Ordering.by[(Int, Int), Int](_._2))
  	)
  } 

  def solve(b: Array[Int]) =
    b.tail.foldLeft(Array((1, 0), (b.head, 0))) {
      case (current, bElement) => iterate(current, bElement)
    }

  def printSolution(sol: Array[(Int, Int)]) = {
    println(sol.max(Ordering.by[(Int, Int), Int](_._2))._2)
  }

  def getInts(input: String) = input.split(" ") map (x => x.toInt)

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val testCaseNr = readLine.toInt
    (1 to testCaseNr).foreach(_ => {
      val n = readLine.toInt
      val b = getInts(readLine)

      printSolution(solve(b))
    })
  }
}