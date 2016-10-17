package exploringStrings

import scala.io.StdIn._

/**
  * Created by horiaradu on 16/10/2016.
  */
class Key(val currentWordLength: Long, val availableLettersLessThanCurrent: Long, val availableLettersGreaterThanCurrent: Long, val currentSequenceLength: Long)

class StringSequenceCounter(val alphabetSize: Long, val sequenceLengths: Seq[Long]) {
  var results = Map[Key, Long]()

  def p(wordLength: Long, sequenceLength: Long): Long = {
    def takeLessThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Seq[Long] =
      (1L to availableLettersLessThanCurrent)
        .map(choice => {
          val nextWordLength = currentWordLength + 1
          val nextAvailableLess = choice - 1
          val nextAvailableGreater = availableLettersLessThanCurrent - choice + availableLettersGreaterThanCurrent
          numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength)
        })


    def takeGreaterThanCurrent(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Seq[Long] =
      ((1L + availableLettersLessThanCurrent) to (availableLettersLessThanCurrent + availableLettersGreaterThanCurrent))
        .map(choice => {
          val nextWordLength = currentWordLength + 1
          val nextAvailableLess = choice - 1
          val nextAvailableGreater = availableLettersGreaterThanCurrent + availableLettersLessThanCurrent - choice
          numberOfSequences(nextWordLength, nextAvailableLess, nextAvailableGreater, currentSequenceLength + 1)
        })


    def computeNumberOfSequences(currentWordLength: Long, availableLettersLessThanCurrent: Long, availableLettersGreaterThanCurrent: Long, currentSequenceLength: Long): Long = {
      if (wordLength - currentWordLength < sequenceLength - currentSequenceLength) 0
      else if (currentWordLength == wordLength) {
        if (currentSequenceLength == sequenceLength) 1
        else 0
      } else {
        val leftCount = takeLessThanCurrent(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength).sum
        val rightCount = takeGreaterThanCurrent(currentWordLength, availableLettersLessThanCurrent, availableLettersGreaterThanCurrent, currentSequenceLength).sum
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