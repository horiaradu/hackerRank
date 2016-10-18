package exploringStrings

import scala.collection.mutable
import scala.io.StdIn._

/**
  * Created by horiaradu on 16/10/2016.
  */
class Key(val currentWordLength: Long, val availableLettersLessThanCurrent: Long,
          val availableLettersGreaterThanCurrent: Long, val currentSequenceLength: Long,
          val wordLength: Long, val sequenceLength: Long) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Key]

  override def equals(other: Any): Boolean = other match {
    case that: Key =>
      (that canEqual this) &&
        currentWordLength == that.currentWordLength &&
        availableLettersLessThanCurrent == that.availableLettersLessThanCurrent &&
        availableLettersGreaterThanCurrent == that.availableLettersGreaterThanCurrent &&
        currentSequenceLength == that.currentSequenceLength &&
        sequenceLength == that.sequenceLength
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength, wordLength, sequenceLength)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class StringSequenceCounter(val alphabetSize: Long, val sequenceLengths: Seq[Long]) {
  var results = mutable.HashMap[Key, BigDecimal]()

  def p(wordLength: Long, sequenceLength: Long): BigDecimal = {
    def takeLessThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): BigDecimal = {
      var total = BigDecimal(0)
      for (choice <- 1L to availableLettersLessThanCurrent) {
        val nextWordLength = currentWordLength + 1
        val nextAvailableLess = choice - 1
        val nextAvailableGreater = availableLettersLessThanCurrent - choice + availableLettersGreaterThanCurrent
        total += numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength)
      }
      total
    }

    def takeGreaterThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): BigDecimal = {
      var total = BigDecimal(0)
      for (choice <- (1L + availableLettersLessThanCurrent) to (availableLettersLessThanCurrent + availableLettersGreaterThanCurrent)) {
        val nextWordLength = currentWordLength + 1
        val nextAvailableLess = choice - 1
        val nextAvailableGreater = availableLettersGreaterThanCurrent + availableLettersLessThanCurrent - choice
        total += numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength + 1)
      }
      total
    }


    def computeNumberOfSequences(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): BigDecimal = {
      if (wordLength - currentWordLength < sequenceLength - currentSequenceLength) 0
      else if (currentWordLength == wordLength) {
        if (currentSequenceLength == sequenceLength) 1
        else 0
      } else {
        val leftCount = takeLessThanCurrent(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength)
        val rightCount = takeGreaterThanCurrent(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength)
        leftCount + rightCount
      }
    }

    def numberOfSequences(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): BigDecimal = {
      val key = new Key(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength, wordLength, sequenceLength)
      if (!results.contains(key)) {
        results += key -> computeNumberOfSequences(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength)
      }

      results(key)
    }


    if (wordLength > alphabetSize) 0
    else {
      val counts = for (choice <- 1L to alphabetSize) yield numberOfSequences(1, choice - 1, alphabetSize - choice, 0)
      counts.sum
    }
  }

  def maxP(sequenceLength: Long): BigDecimal = {
    if (sequenceLength >= alphabetSize) 0L
    else {
      var pMax = BigDecimal(0)
      val minN = math.floor(50 * alphabetSize / 100.0).toLong
      val maxN = math.ceil(100 * alphabetSize / 100.0).toLong
      for (wordLength <- minN to maxN) {
        val currentP = p(wordLength, sequenceLength)
        if (currentP > pMax) pMax = currentP
      }
      pMax
    }
  }

  def solve(): BigDecimal = {
    var sum = BigDecimal(0)
    for (sequenceLength <- sequenceLengths) {
      val max = maxP(sequenceLength)
      sum += max
    }
    sum
  }
}

object Solution {
  def getLongs(input: String) = input.split(" ") map (x => x.toLong)

  def main(args: Array[String]) {
    val firstLine = getLongs(readLine)
    val alphabetSize = firstLine(0)

    val sequences = getLongs(readLine)
    val counter = new StringSequenceCounter(alphabetSize, sequences)
    println(counter.solve())
  }
}