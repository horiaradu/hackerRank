package hacker

import scala.math.pow

object Solution {

  0 until 2                                       //> res0: scala.collection.immutable.Range = Range(0, 1)
  def unfairness(xs: List[Int]) =
    xs.last - xs.head                             //> unfairness: (xs: List[Int])Int

  val x = List(10, 100, 300, 200, 1000, 20, 30).sorted
                                                  //> x  : List[Int] = List(10, 20, 30, 100, 200, 300, 1000)

  val k = 3                                       //> k  : Int = 3

  val a = (k to x.size).map(index =>
    x.slice(index - k, index))                    //> a  : scala.collection.immutable.IndexedSeq[List[Int]] = Vector(List(10, 20, 
                                                  //| 30), List(20, 30, 100), List(30, 100, 200), List(100, 200, 300), List(200, 3
                                                  //| 00, 1000))

  val b = a.map(slice =>
    (slice, unfairness(slice)))                   //> b  : scala.collection.immutable.IndexedSeq[(List[Int], Int)] = Vector((List(
                                                  //| 10, 20, 30),20), (List(20, 30, 100),80), (List(30, 100, 200),170), (List(100
                                                  //| , 200, 300),200), (List(200, 300, 1000),800))

  b.min(Ordering.by[(List[Int], Int), Int](_._2)) //> res1: (List[Int], Int) = (List(10, 20, 30),20)

  def findKs(xs: List[Int], k: Int) = {
    var min = Int.MaxValue
    (k until xs.size).foreach(index =>
      if (xs(index - 1) - xs(index - k) < min) {
        min = xs(index - 1) - xs(index - k)
      })

    min
  }                                               //> findKs: (xs: List[Int], k: Int)Int

  x.sorted                                        //> res2: List[Int] = List(10, 20, 30, 100, 200, 300, 1000)

  (k until x.size).foreach(index =>
    println(x(index - 1) - x(index - k)))         //> 20
                                                  //| 80
                                                  //| 170
                                                  //| 200

  findKs(x.sorted, k)                             //> res3: Int = 20
  //b.max(Ordering.by[List[Int], Int](_._2))

  //a.max(slice =>
  //  slice.foldLeft(1) {
  //    (partial, elem) => partial + elem
  //  })

}