package hacker

import scala.io.StdIn.readLong
import scala.io.StdIn.readLine

object Solution {
  def getLongs(input: String) = input.split(" ") map (x => x.toLong)

  def pmean(values: Array[Long]) =
    (values zipWithIndex).foldLeft(0L) {
      case (result, (element, index)) => (index + 1) * element + result
    }

  def sumOfElements(values: Array[Long]) = values.foldLeft(0L) {
    case (result, element) => result + element
  }

  def iterate(values: Array[Long], pMean: Long, sumOfElement: Long) = {
    val length = values.length
    val resultPMean = pMean - values(length - 1) * length + sumOfElement
    val newValues = Array.concat(Array(values(length - 1)), values.slice(0, length - 1))
    (newValues, resultPMean)
  }

  def maxPMean(values: Array[Long]) = {
    def _maxPMean(currentValues: Array[Long], currentPMean: Long, maxPMean: Long, sum: Long, iterationCount: Long): Long =
      if (iterationCount == currentValues.length) maxPMean
      else iterate(currentValues, currentPMean, sum) match {
        case (nextValues, nextPMean) =>
          if (nextPMean > maxPMean) _maxPMean(nextValues, nextPMean, nextPMean, sum, iterationCount + 1)
          else _maxPMean(nextValues, nextPMean, maxPMean, sum, iterationCount + 1)
      }

    val pMean = pmean(values)
    _maxPMean(values, pMean, pMean, sumOfElements(values), 0)
  }

  def main(args: Array[String]) {
    readLong
    println(maxPMean(getLongs(readLine)))
  }
}