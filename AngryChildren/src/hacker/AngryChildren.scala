package hacker

import scala.io.StdIn.readInt

object AngryChildren {
  def unfairness(xs: Seq[Int]) =
    xs.last - xs.head

  def produceSlices(xs: Seq[Int], k: Int) =
    (k to xs.size) map (index =>
      xs.slice(index - k, index))

  def findKs(xs: Seq[Int], k: Int) =
    produceSlices(xs.sorted, k) map (slice => unfairness(slice)) min

  def main(args: Array[String]) {
    val n = readInt
    val k = readInt
    val xs = (0 until n) map (_ => readInt)

    println(findKs(xs, k))
  }

  //  def findKs(xs: Seq[Int], k: Int) = {
  //    var min = Int.MaxValue
  //    (k until xs.size).foreach(index =>
  //      if (xs(index - 1) - xs(index - k) < min) {
  //        min = xs(index - 1) - xs(index - k)
  //      })
  //
  //    min
  //  }
  //
  //  def main(args: Array[String]) {
  //    val n = readInt
  //    val k = readInt
  //    val xs = (0 until n) map (_ => readInt)
  //
  //    println(findKs(xs.sorted, k))
  //  }
}