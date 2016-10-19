package candies

import scala.io.StdIn._

/**
  * Created by horiaradu on 18/10/2016.
  */
object Solution {
  def candies(kids: Seq[Long]): Long = {
    val candies = new Array[Long](kids.length)

    for (i <- kids.indices) {
      if (i == 0 || kids(i - 1) >= kids(i)) candies(i) = 1
      else candies(i) = candies(i - 1) + 1
    }

    for {
      i <- kids.length - 1 to 1 by -1
      if kids(i - 1) > kids(i) && candies(i - 1) <= candies(i)
    } candies(i - 1) = candies(i) + 1

    candies.sum
  }

  def main(args: Array[String]) {
    val numberOfKids = readLong
    val kids = (1L to numberOfKids).map(_ => readLong)
    println(candies(kids))
  }
}
