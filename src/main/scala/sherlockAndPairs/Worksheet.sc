package hacker

object Worksheet {
  def factorial(number: Int) = {
    def _factorial(number: Int, result: Int): Int =
      if (number == 0) result
      else _factorial(number - 1, result * number)
    _factorial(number, 0)
  }                                               //> factorial: (number: Int)Int

  def pairsCount(noElements: Int) = noElements * (noElements - 1)
                                                  //> pairsCount: (noElements: Int)Int

  pairsCount(3)                                   //> res0: Int = 6

  def buildCountMap(numbers: List[Int]) =
    numbers.foldLeft(Map[Int, Int]()) {
      case (currentMap, number) => currentMap.get(number) match {
        case Some(count) => currentMap + (number -> (count + 1))
        case None => currentMap + (number -> 1)
      }
    }                                             //> buildCountMap: (numbers: List[Int])scala.collection.immutable.Map[Int,Int]

  def countPairs(countMap: Map[Int, Int]) =
    countMap.foldLeft(0) {
      case (result, (key, value)) => if (value > 1) result + pairsCount(value) else result
    }                                             //> countPairs: (countMap: Map[Int,Int])Int

  buildCountMap(List(1, 2, 3, 1, 2, 1))           //> res1: scala.collection.immutable.Map[Int,Int] = Map(1 -> 3, 2 -> 2, 3 -> 1)
                                                  //| 
  countPairs(buildCountMap(List(1, 2, 3, 1, 2, 1)))
                                                  //> res2: Int = 8
  countPairs(buildCountMap(List(1, 2, 3)))        //> res3: Int = 0
  countPairs(buildCountMap(List(1, 1, 3)))        //> res4: Int = 2

}