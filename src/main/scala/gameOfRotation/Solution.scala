package gameOfRotation

import scala.io.StdIn.{readInt, readLine}

object Solution {
  def getLongs(input: String) = input.split(" ") map (x => x.toLong)

  def pmean(values: Array[Long]) =
    (values zipWithIndex).foldLeft(0L) {
      case (result, (element, index)) => (index + 1) * element + result
    }

  def sumOfElements(values: Array[Long]) = values.foldLeft(0L) {
    case (result, element) => result + element
  }

  def maxPMean(length: Int, values: Array[Long]) = {
    def iterate(lastIndex: Int, pMean: Long, sumOfElement: Long) = {
      val resultPMean = pMean - values(lastIndex) * length + sumOfElement
      (lastIndex - 1, resultPMean)
    }

    def _maxPMean(lastIndex: Int, currentPMean: Long, max: Long, sum: Long): Long =
      if (lastIndex == -1) max
      else iterate(lastIndex, currentPMean, sum) match {
        case (nextLastIndex, nextPMean) =>
          if (nextPMean > max) _maxPMean(nextLastIndex, nextPMean, nextPMean, sum)
          else _maxPMean(nextLastIndex, nextPMean, max, sum)
      }

    val pMean = pmean(values)
    _maxPMean(length - 1, pMean, pMean, sumOfElements(values))
  }

  def main(args: Array[String]) {
    println(maxPMean(readInt, getLongs(readLine)))
  }
}