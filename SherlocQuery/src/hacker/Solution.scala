package hacker

import scala.io.StdIn.readLine
import scala.math.pow

object Solution {
  /**
   * If an element is present multiple times in b, then the same positions in a will be multiplied with the corresponding elements in c.
   * It is possible to calculate beforehand the product of the elements in c so that we don't iterate multiple times over a.
   *
   * Returns a map with key: the unique elements in b, and value: the product of the elements in c corresponding to the indexes of that particular
   * element in b.
   */
  def calculateMultipliers(b: Array[Int], c: Array[Int]) =
    (b zip c) groupBy {
      case (b, c) => b
    } map {
      case (b, pairs) => (b, pairs map {
        case (_, c) => c
      })
    } map {
      case (b, cs) => (b, cs.foldLeft(1) {
        case (partialResult, c) => module(partialResult.toLong * c)
      })
    }

  def module(x: Long) = (x % (pow(10, 9).toInt + 7)).toInt

  def iterate(n: Int, b: Int, c: Int, a: Array[Int]) = {
    (1 to n / b) foreach (index =>
      a(index * b - 1) = module(c.toLong * a(index * b - 1)))

    a
  }

  def calculate(n: Int, m: Int, a: Array[Int], multipliers: Map[Int, Int]) =
    multipliers.foldLeft(a) {
      case (partialResult, multiplierPair) => multiplierPair match {
        case (b, multiplier) => iterate(n, b, multiplier, partialResult)
      }
    }

  def getInts(input: String) = input.split(" ") map (x => x.toInt)

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val firstLine = getInts(readLine)
    val a = getInts(readLine)
    val b = getInts(readLine)
    val c = getInts(readLine)

    calculate(firstLine(0), firstLine(1), a, calculateMultipliers(b, c)) map (x => print(x + " "))
  }
}