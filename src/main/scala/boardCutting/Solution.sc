package hacker

import scala.math.pow

object Solution {
  def horizontalCosts = List(2, 1, 3, 1, 4).sorted(Ordering[Int].reverse)
                                                  //> horizontalCosts: => List[Int]
  horizontalCosts                                 //> res0: List[Int] = List(4, 3, 2, 1, 1)
  def verticalCosts = List(4, 1, 2)               //> verticalCosts: => List[Int]

  def module(x: Long) = (x % (pow(10, 9).toInt + 7)).toInt
                                                  //> module: (x: Long)Int

  def _computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int], noHorizontalCuts: Int, noVerticalCuts: Int, cost: Int): Int = {
    def cutVertically(verticalCost: Int, remainingCosts: List[Int]) = _computeMinCost(horizontalCosts, remainingCosts, noHorizontalCuts,
      noVerticalCuts + 1, module(cost + verticalCost * noHorizontalCuts))
    def cutHorizontally(horizontalCost: Int, remainingCosts: List[Int]) = _computeMinCost(remainingCosts, verticalCosts,
      noHorizontalCuts + 1, noVerticalCuts, module(cost + horizontalCost * noVerticalCuts))

    (horizontalCosts, verticalCosts) match {
      case (Nil, Nil) => cost
      case (head :: tail, Nil) => cutHorizontally(head, tail)
      case (Nil, head :: tail) => cutVertically(head, tail)
      case (horizontalHead :: horizontalTail, verticalHead :: verticalTail) =>
        if (horizontalHead > verticalHead) cutHorizontally(horizontalHead, horizontalTail)
        else cutVertically(verticalHead, verticalTail)
    }
  }                                               //> _computeMinCost: (horizontalCosts: List[Int], verticalCosts: List[Int], noH
                                                  //| orizontalCuts: Int, noVerticalCuts: Int, cost: Int)Int

  def computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int]) =
    _computeMinCost(horizontalCosts, verticalCosts, 1, 1, 0)
                                                  //> computeMinCost: (horizontalCosts: List[Int], verticalCosts: List[Int])Int

  computeMinCost(List(2), List(1))                //> res1: Int = 4

  computeMinCost(List(2), List(1, 1))             //> res2: Int = 6

  computeMinCost(List(2, 1), List(1, 1))          //> res3: Int = 9

  computeMinCost(horizontalCosts, verticalCosts)  //> res4: Int = 42

  computeMinCost(List.empty, List.empty)          //> res5: Int = 0

  computeMinCost(List(2, 1, 3), List.empty)       //> res6: Int = 6
}