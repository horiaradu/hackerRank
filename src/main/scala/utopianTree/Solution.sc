package utopianTree

object Solution {

  def callMethod(count: Int): Unit = {
    if (count != 0) {
      println(UtopianTree calculateHeight (readLine.toInt))
      callMethod(count - 1)
    }
  }                                               //> callMethod: (count: Int)Unit

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    callMethod(readLine.toInt)
  }                                               //> main: (args: Array[String])Unit
}