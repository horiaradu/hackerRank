package hacker

import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.math.pow

object BoardCutting {
  def module(x: Long) = (x % (pow(10, 9).toInt + 7)).toInt

  def removeElement[T](list: List[T], x: T): List[T] =
    list.foldLeft(List.empty[T], false) {
      case ((result, seen), element) =>
        if (x.equals(element) && !seen) (result, true)
        else (element :: result, seen)
    }._1

  def _computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int], noHorizontalCuts: Int, noVerticalCuts: Int,
    totalCutsCount: Int, cost: Int): Int = {
    def cutVertically(verticalCost: Int) = _computeMinCost(horizontalCosts, removeElement(verticalCosts, verticalCost), noHorizontalCuts,
      noVerticalCuts + 1, totalCutsCount, module(cost + verticalCost * noHorizontalCuts))
    def cutHorizontally(horizontalCost: Int) = _computeMinCost(removeElement(horizontalCosts, horizontalCost), verticalCosts,
      noHorizontalCuts + 1, noVerticalCuts, totalCutsCount, module(cost + horizontalCost * noVerticalCuts))

    if (noHorizontalCuts + noVerticalCuts == totalCutsCount) cost
    else if (horizontalCosts.isEmpty) cutVertically(verticalCosts.max)
    else if (verticalCosts.isEmpty) cutHorizontally(horizontalCosts.max)
    else (horizontalCosts.max, verticalCosts.max) match {
      case (horizontalMax, verticalMax) =>
        if (horizontalMax > verticalMax) cutHorizontally(horizontalMax)
        else cutVertically(verticalMax)
    }
  }

  def computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int]) =
    _computeMinCost(horizontalCosts, verticalCosts, 1, 1, verticalCosts.size + horizontalCosts.size + 2, 0)

  def getInts(input: String) = (input.split(" ") map (x => x.toInt)).toList

  def main(args: Array[String]) {
    val noTestCases = readInt
    (1 to noTestCases).foreach(_ => {
      getInts(readLine)
      println(computeMinCost(getInts(readLine), getInts(readLine)))
    })
  }
}