package exploringStrings

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by horiaradu on 16/10/2016.
  */
@RunWith(classOf[JUnitRunner])
class StringSequenceCounterSuite extends FunSuite {
  val twoLetterAlphabetCounter = new StringSequenceCounter(2, Seq())
  val threeLetterAlphabetCounter = new StringSequenceCounter(3, Seq())
  val fourLetterAlphabetCounter = new StringSequenceCounter(4, Seq())

  test("twoLetterAlphabetCounter.p(1, 0) should be 2") {
    assert(twoLetterAlphabetCounter.p(1, 0) === 2)
  }

  test("twoLetterAlphabetCounter.p(1, 1) should be 1") {
    assert(twoLetterAlphabetCounter.p(1, 1) === 0)
  }

  test("twoLetterAlphabetCounter.p(2, 0) should be 1") {
    assert(twoLetterAlphabetCounter.p(2, 0) === 1)
  }

  test("twoLetterAlphabetCounter.p(2, 1) should be 2") {
    assert(twoLetterAlphabetCounter.p(2, 1) === 1)
  }

  test("twoLetterAlphabetCounter.p(3, 2) should be 0") {
    assert(twoLetterAlphabetCounter.p(3, 2) === 0)
  }

  test("twoLetterAlphabetCounter.p(2, 3) should be 0") {
    assert(twoLetterAlphabetCounter.p(2, 3) === 0)
  }

  test("twoLetterAlphabetCounter") {
    assert(new StringSequenceCounter(2, Seq(0, 1, 2)).solve() === 3)
  }

  test("threeLetterAlphabetCounter.p(1, 0) should be 3") {
    assert(threeLetterAlphabetCounter.p(1, 0) === 3)
  }

  test("threeLetterAlphabetCounter.p(1, 1) should be 0") {
    assert(threeLetterAlphabetCounter.p(1, 1) === 0)
  }

  test("threeLetterAlphabetCounter.p(1, 2) should be 0") {
    assert(threeLetterAlphabetCounter.p(1, 2) === 0)
  }

  test("threeLetterAlphabetCounter.p(2, 0) should be 3") {
    assert(threeLetterAlphabetCounter.p(2, 0) === 3)
  }

  test("threeLetterAlphabetCounter.p(2, 1) should be 3") {
    assert(threeLetterAlphabetCounter.p(2, 1) === 3)
  }

  test("threeLetterAlphabetCounter.p(2, 2) should be 0") {
    assert(threeLetterAlphabetCounter.p(2, 2) === 0)
  }

  test("threeLetterAlphabetCounter.p(3, 0) should be 1") {
    assert(threeLetterAlphabetCounter.p(3, 0) === 1)
  }

  test("threeLetterAlphabetCounter.p(3, 1) should be 1") {
    assert(threeLetterAlphabetCounter.p(3, 1) === 4)
  }

  test("threeLetterAlphabetCounter.p(3, 2) should be 1") {
    assert(threeLetterAlphabetCounter.p(3, 2) === 1)
  }

  test("threeLetterAlphabetCounter.p(3, 3) should be 0") {
    assert(threeLetterAlphabetCounter.p(3, 3) === 0)
  }

  test("threeLetterAlphabetCounter") {
    assert(new StringSequenceCounter(3, Seq(0, 1, 2, 3)).solve() === 8)
  }

  test("fourLetterAlphabetCounter.p(4, 3) should be 1") {
    assert(fourLetterAlphabetCounter.p(4, 3) === 1)
  }

  test("fourLetterAlphabetCounter.p(4, 2) should be 11") {
    assert(fourLetterAlphabetCounter.p(4, 2) === 11)
  }

  test("fourLetterAlphabetCounter.p(4, 1) should be 11") {
    assert(fourLetterAlphabetCounter.p(4, 1) === 11)
  }

  test("fourLetterAlphabetCounter.p(4, 0) should be 1") {
    assert(fourLetterAlphabetCounter.p(4, 0) === 1)
  }

  test("fourLetterAlphabetCounter") {
    assert(new StringSequenceCounter(4, Seq(0, 1, 2, 3, 4)).solve() === 34)
  }

  test("26LetterAlphabetCounter") {
    assert(new StringSequenceCounter(26, 0L to 26L).solve() === BigDecimal("458021678418266656880288754"))
  }
}
