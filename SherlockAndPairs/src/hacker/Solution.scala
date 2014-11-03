package hacker

import scala.io.StdIn.readLong
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object Solution {
  def pairsCount(noElements: Long) = noElements * (noElements - 1)

  def buildCountMap(numbers: List[Long]) =
    numbers.foldLeft(Map[Long, Long]()) {
      case (currentMap, number) => currentMap.get(number) match {
        case Some(count) => currentMap + (number -> (count + 1))
        case None => currentMap + (number -> 1)
      }
    }

  def countPairs(countMap: Map[Long, Long]) =
    countMap.foldLeft(0L) {
      case (result, (key, value)) => if (value > 1) result + pairsCount(value) else result
    }

  def getLongs(input: String) = (input.split(" ") map (x => x.toLong)).toList

  def main(args: Array[String]) {
    val noTestCases = readInt
    (1 to noTestCases).foreach(_ => {
      readLine
      println(countPairs(buildCountMap(getLongs(readLine))))
    })
  }
}