import exploringStrings.StringSequenceCounter

object session {
  val twoLetterAlphabetCounter = new StringSequenceCounter(2, Seq())

  twoLetterAlphabetCounter.p(1, 0)
  twoLetterAlphabetCounter.p(1, 1)
  twoLetterAlphabetCounter.p(2, 0)
  twoLetterAlphabetCounter.p(2, 1)

  val threeLetterAlphabetCounter = new StringSequenceCounter(2, Seq())
  threeLetterAlphabetCounter.p(1, 0)
  threeLetterAlphabetCounter.p(1, 1)
  threeLetterAlphabetCounter.p(1, 2)
  threeLetterAlphabetCounter.p(2, 0)
  threeLetterAlphabetCounter.p(2, 1)
  threeLetterAlphabetCounter.p(2, 2)
  threeLetterAlphabetCounter.p(3, 0)
  threeLetterAlphabetCounter.p(3, 1) //ACB BAC BCA CAB
  threeLetterAlphabetCounter.p(3, 2)
  threeLetterAlphabetCounter.p(3, 3)
}