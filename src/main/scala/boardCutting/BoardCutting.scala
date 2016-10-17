package boardCutting

import scala.io.StdIn.{readInt, readLine}
import scala.math.pow

object BoardCutting {
  def module(x: Long) = (x % (pow(10, 9).toInt + 7)).toInt

  def computeCutCost(cost: Int, cutCost: Int, noPieces: Int) =
    module(cutCost.toLong * noPieces.toLong + cost.toLong)

  def _computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int], noHorizontalCuts: Int, noVerticalCuts: Int, cost: Int): Int = {
    def cutVertically(verticalCost: Int, remainingCosts: List[Int]) = _computeMinCost(horizontalCosts, remainingCosts, noHorizontalCuts,
      noVerticalCuts + 1, computeCutCost(cost, verticalCost, noHorizontalCuts))
    def cutHorizontally(horizontalCost: Int, remainingCosts: List[Int]) = _computeMinCost(remainingCosts, verticalCosts,
      noHorizontalCuts + 1, noVerticalCuts, computeCutCost(cost, horizontalCost, noVerticalCuts))

    (horizontalCosts, verticalCosts) match {
      case (Nil, Nil) => cost
      case (head :: tail, Nil) => cutHorizontally(head, tail)
      case (Nil, head :: tail) => cutVertically(head, tail)
      case (horizontalHead :: horizontalTail, verticalHead :: verticalTail) =>
        if (horizontalHead > verticalHead) cutHorizontally(horizontalHead, horizontalTail)
        else cutVertically(verticalHead, verticalTail)
    }
  }

  def computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int]) =
    _computeMinCost(horizontalCosts, verticalCosts, 1, 1, 0)

  def getInts(input: String) = (input.split(" ") map (x => x.toInt)).toList

  def main(args: Array[String]) {
    val noTestCases = readInt
    (1 to noTestCases).foreach(_ => {
      getInts(readLine)
      println(computeMinCost(getInts(readLine).sorted(Ordering[Int].reverse), getInts(readLine).sorted(Ordering[Int].reverse)))
    })
  }
}