package hacker

import scala.math.pow

object Solution2 {
  def iterateFunctional(n: Int, b: Int, c: Int, a: Array[Int]) =
    (a zipWithIndex) map {
      case (elem, index) =>
        if ((index + 1) % b == 0) module(elem * c)
        else module(elem)
    }                                             //> iterateFunctional: (n: Int, b: Int, c: Int, a: Array[Int])Array[Int]

  def calculate(n: Int, m: Int, a: Array[Int], b: Array[Int], c: Array[Int]) =
    (b zip c).foldLeft(a) {
      case (partialResult, bcPair) => bcPair match {
        case (bElem, cElem) => iterate(n, bElem, cElem, partialResult)
      }
    }                                             //> calculate: (n: Int, m: Int, a: Array[Int], b: Array[Int], c: Array[Int])Arra
                                                  //| y[Int]

  def module(x: Long) = (x % (math.pow(10, 9).toInt + 7)).toInt
                                                  //> module: (x: Long)Int

  def iterate(n: Int, b: Int, c: Int, a: Array[Int]) = {
    (1 to n / b) foreach (index =>
      a(index * b - 1) = module(c.toLong * a(index * b - 1)))

    a
  }                                               //> iterate: (n: Int, b: Int, c: Int, a: Array[Int])Array[Int]

  iterate(5, 2, 3, Array(1, 1, 1, 1, 1))          //> res0: Array[Int] = Array(1, 3, 1, 3, 1)

  calculate(5, 1, Array(1, 1, 1, 1, 1), Array(2), Array(3))
                                                  //> res1: Array[Int] = Array(1, 3, 1, 3, 1)
  calculate(4, 3, Array(1, 2, 3, 4), Array(1, 2, 3), Array(13, 29, 71))
                                                  //> res2: Array[Int] = Array(13, 754, 2769, 1508)

  calculate(4, 1, Array(1, 2, 3, 4), Array(1), Array(13))
                                                  //> res3: Array[Int] = Array(13, 26, 39, 52)

  calculate(4, 2, Array(1, 2, 3, 4), Array(1, 2), Array(13, 29))
                                                  //> res4: Array[Int] = Array(13, 754, 39, 1508)

  calculate(4, 1, Array(pow(10, 9).toInt + 8, 2, 3, 4), Array(3), Array(13))
                                                  //> res5: Array[Int] = Array(1000000008, 2, 39, 4)

  calculate(5, 2, Array(1, 1, 1, 1, 1), Array(2, 5), Array(2, 2))
                                                  //> res6: Array[Int] = Array(1, 2, 1, 2, 2)

  49650.toLong * 66946                            //> res7: Long = 3323868900

  def calculateElement(c: Int, a: Int) = module(c.toLong * a)
                                                  //> calculateElement: (c: Int, a: Int)Int
  calculateElement(2, 3)                          //> res8: Int = 6
  
  Array(1, 1, 1, 2).groupBy(x => x).map {
    case (elem, array) => (elem, array.size)
  }                                               //> res9: scala.collection.immutable.Map[Int,Int] = Map(2 -> 1, 1 -> 3)

  (Array(1, 1, 2, 2) zip Array(1, 2, 3, 4)) groupBy {
    case (b, c) => b
  } map {
    case (b, pairs) => (b, pairs map {
    	case (_, c) => c
    })
  } map {
  	case (b, cs) => (b, cs.foldLeft(1) {
  		case (partialResult, c) => partialResult * c
  	})
  }                                               //> res10: scala.collection.immutable.Map[Int,Int] = Map(2 -> 12, 1 -> 2)
}