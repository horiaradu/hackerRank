package hacker

import scala.io.StdIn.readLine
import scala.math.pow

object SherlockQuery {
  def iterateFunctional(n: Int, b: Int, c: Int, a: Array[Int]) =
    (a zipWithIndex) map {
      case (elem, index) =>
        if ((index + 1) % b == 0) module(elem * c)
        else module(elem)
    }

  def calculateFunctional(n: Int, m: Int, a: Array[Int], b: Array[Int], c: Array[Int]) =
    (b zip c).foldLeft(a) {
      case (partialResult, bcPair) => bcPair match {
        case (bElem, cElem) => iterate(n, bElem, cElem, partialResult)
      }
    }

  def module(x: Long) = (x % (pow(10, 9).toInt + 7)).toInt
  
  def iterate(n: Int, b: Int, c: Int, a: Array[Int]) = {
    (1 to n / b) foreach (index =>
      a(index * b - 1) = module(c.toLong * a(index * b - 1)))

    a
  }

  def calculate(n: Int, m: Int, a: Array[Int], b: Array[Int], c: Array[Int]) =
    (0 until m).foldLeft(a) {
      case (partialResult, index) => iterate(n, b(index), c(index), partialResult)
    }

  def getInts(input: String) = input.split(" ") map (x => x.toInt)

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val firstLine = getInts(readLine)
    val a = getInts(readLine)
    val b = getInts(readLine)
    val c = getInts(readLine)

    calculate(firstLine(0), firstLine(1), a, b, c) map (x => print(x + " "))
  }
}