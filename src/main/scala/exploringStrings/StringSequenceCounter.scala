package exploringStrings

import scala.collection.mutable
import scala.io.StdIn._

/**
  * Created by horiaradu on 16/10/2016.
  */
class Key(val currentWordLength: Long, val availableLettersLessThanCurrent: Long, val availableLettersGreaterThanCurrent: Long, val currentSequenceLength: Long)

class StringSequenceCounter(val alphabetSize: Long, val sequenceLengths: Seq[Long]) {
  var results = mutable.HashMap[Key, Long]()

  def p(wordLength: Long, sequenceLength: Long): Long = {
    def takeLessThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Long = {
      var total = 0L
      for (choice <- 1L to availableLettersLessThanCurrent) {
        val nextWordLength = currentWordLength + 1
        val nextAvailableLess = choice - 1
        val nextAvailableGreater = availableLettersLessThanCurrent - choice + availableLettersGreaterThanCurrent
        total += numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength)
      }
      total
    }

    def takeGreaterThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Long = {
      var total = 0L
      for (choice <- (1L + availableLettersLessThanCurrent) to (availableLettersLessThanCurrent + availableLettersGreaterThanCurrent)) {
        val nextWordLength = currentWordLength + 1
        val nextAvailableLess = choice - 1
        val nextAvailableGreater = availableLettersGreaterThanCurrent + availableLettersLessThanCurrent - choice
        total += numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength + 1)
      }
      total
    }


    def computeNumberOfSequences(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Long = {
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

    def numberOfSequences(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Long = {
      val key = new Key(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength)
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

  def maxP(sequenceLength: Long) = {
    val ps = for (wordLength <- 1L to alphabetSize) yield p(wordLength, sequenceLength)
    ps.max
  }

  def solve(): Long = {
    var sum = 0L
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