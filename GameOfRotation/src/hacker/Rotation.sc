package hacker

object Rotation {
  def pmean(values: Array[Long]) =
    (values zipWithIndex).foldLeft(0L) {
      case (result, (element, index)) => (index + 1) * element + result
    }                                             //> pmean: (values: Array[Long])Long

  def sumOfElements(values: Array[Long]) = values.foldLeft(0L) {
    case (result, element) => result + element
  }                                               //> sumOfElements: (values: Array[Long])Long

  def iterate(values: Array[Long], pMean: Long, sumOfElement: Long) = {
    val length = values.length
    val resultPMean = pMean - values(length - 1) * length + sumOfElement
    val newValues = Array.concat(Array(values(length - 1)), values.slice(0, length - 1))
    (newValues, resultPMean)
  }                                               //> iterate: (values: Array[Long], pMean: Long, sumOfElement: Long)(Array[Long],
                                                  //|  Long)

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
  }                                               //> maxPMean: (values: Array[Long])Long

  val x = Array(20L, 30L, 10L)                    //> x  : Array[Long] = Array(20, 30, 10)
  val pMeanVal = pmean(x)                         //> pMeanVal  : Long = 110
  val sum = sumOfElements(x)                      //> sum  : Long = 60
  iterate(x, pmean(x), sumOfElements(x))          //> res0: (Array[Long], Long) = (Array(10, 20, 30),140)
  maxPMean(x)                                     //> res1: Long = 140

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}