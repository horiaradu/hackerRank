package hacker

object Solution {
  def horizontalCosts = List(2, 1, 3, 1, 4)       //> horizontalCosts: => List[Int]
  def verticalCosts = List(4, 1, 2)               //> verticalCosts: => List[Int]

  def removeElement[T](list: List[T], x: T): List[T] =
    list.foldLeft(List.empty[T], false) {
      case ((result, seen), element) =>
        if (x.equals(element) && !seen) (result, true)
        else (element :: result, seen)
    }._1                                          //> removeElement: [T](list: List[T], x: T)List[T]

  removeElement(List(3, 1, 2, 4, 2, 5), 2)        //> res0: List[Int] = List(5, 2, 4, 1, 3)

  def _computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int], noHorizontalCuts: Int, noVerticalCuts: Int,
    totalCutsCount: Int, cost: Int): Int = {
    def cutVertically(verticalCost: Int) = _computeMinCost(horizontalCosts, removeElement(verticalCosts, verticalCost), noHorizontalCuts,
      noVerticalCuts + 1, totalCutsCount, cost + verticalCost * noHorizontalCuts)
    def cutHorizontally(horizontalCost: Int) = _computeMinCost(removeElement(horizontalCosts, horizontalCost), verticalCosts,
      noHorizontalCuts + 1, noVerticalCuts, totalCutsCount, cost + horizontalCost * noVerticalCuts)

    if (noHorizontalCuts + noVerticalCuts == totalCutsCount) cost
    else if (horizontalCosts.isEmpty) cutVertically(verticalCosts.max)
    else if (verticalCosts.isEmpty) cutHorizontally(horizontalCosts.max)
    else (horizontalCosts.max, verticalCosts.max) match {
      case (horizontalMax, verticalMax) =>
        if (horizontalMax > verticalMax) cutHorizontally(horizontalMax)
        else cutVertically(verticalMax)
    }
  }                                               //> _computeMinCost: (horizontalCosts: List[Int], verticalCosts: List[Int], noH
                                                  //| orizontalCuts: Int, noVerticalCuts: Int, totalCutsCount: Int, cost: Int)Int
                                                  //| 

  def computeMinCost(horizontalCosts: List[Int], verticalCosts: List[Int]) =
    _computeMinCost(horizontalCosts, verticalCosts, 1, 1, verticalCosts.size + horizontalCosts.size + 2, 0)
                                                  //> computeMinCost: (horizontalCosts: List[Int], verticalCosts: List[Int])Int

  computeMinCost(List(2), List(1))                //> res1: Int = 4

  computeMinCost(List(2), List(1, 1))             //> res2: Int = 6

  computeMinCost(List(2, 1), List(1, 1))          //> res3: Int = 9

  computeMinCost(horizontalCosts, verticalCosts)  //> res4: Int = 42
  
  computeMinCost(List.empty, List.empty)          //> res5: Int = 0
  
  computeMinCost(List(2, 1, 3), List.empty)       //> res6: Int = 6
}