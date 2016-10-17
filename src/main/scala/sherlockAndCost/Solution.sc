import scala.math.abs

object Solution {
  val b = Array(10, 1, 10, 1, 10)                 //> b  : Array[Int] = Array(10, 1, 10, 1, 10)

  def iterate(current: Array[(Int, Int)], b: Int): Array[(Int, Int)] = {
    Array(1, b) map (choice =>
      (current map {
        case (element, sum) => (choice, sum + abs(element - choice))
      }).max(Ordering.by[(Int, Int), Int](_._2)))
  }                                               //> iterate: (current: Array[(Int, Int)], b: Int)Array[(Int, Int)]

  val cur = Array(1, 10) zip Array(0, 0)          //> cur  : Array[(Int, Int)] = Array((1,0), (10,0))
  iterate(cur, 10)                                //> res0: Array[(Int, Int)] = Array((1,9), (10,9))
}