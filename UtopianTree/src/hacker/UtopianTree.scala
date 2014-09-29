package hacker

object UtopianTree {
  def calculateHeight(year: Int): Int =
    if (year == 0) 1
    else if (year % 2 == 1) 2 * calculateHeight(year - 1)
    else 1 + calculateHeight(year - 1)

  def callMethod(count: Int): Unit = {
    if (count != 0) {
      println(UtopianTree calculateHeight (readLine.toInt))
      callMethod(count - 1)
    }
  } //> callMethod: (count: Int)Unit

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    callMethod(readLine.toInt)
  }
}