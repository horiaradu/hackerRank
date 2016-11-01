package betweenTwoSets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by horiaradu on 16/10/2016.
  */
@RunWith(classOf[JUnitRunner])
class BetweenTwoSetsSuite extends FunSuite {
  test("2 = 2 ^ 1") {
    assert(Solution.primeFactors(2) === Map(1 -> 1, 2 -> 1))
  }

  test("4 = 2 ^ 2") {
    assert(Solution.primeFactors(4) === Map(1 -> 1, 2 -> 2))
  }

  test("36 = 2 ^ 2 * 3 ^ 2") {
    assert(Solution.primeFactors(36) === Map(1 -> 1, 3 -> 2, 2 -> 2))
  }

  test("180 = 2 ^ 2 * 3 ^ 2 * 5") {
    assert(Solution.primeFactors(180) === Map(1 -> 1, 5 -> 1, 3 -> 2, 2 -> 2))
  }

  test("7 = 7 ^ 1") {
    assert(Solution.primeFactors(7) === Map(1 -> 1, 7 -> 1))
  }

  test("gcd(8, 24) = 1 ^ 1 * 2 ^ 3") {
    assert(Solution.gcd(Set(8, 24)) === Map(1 -> 1, 2 -> 3))
  }

  test("gcd(1, 2, 3, 4) = 1 ^ 1") {
    assert(Solution.gcd(Set(1, 2, 3, 4)) === Map(1 -> 1))
  }

  test("lcm(8, 24) = 1 ^ 1 * 2 ^ 3 * 3 ^ 1") {
    assert(Solution.lcm(Set(8, 24)) === Map(1 -> 1, 2 -> 3, 3 -> 1))
  }

  test("lcm(1, 2, 3, 4) = 1 ^ 1 * 2 ^ 2 * 3 ^ 1") {
    assert(Solution.lcm(Set(1, 2, 3, 4)) === Map(1 -> 1, 2 -> 2, 3 -> 1))
  }

  test("numbers between {2, 4} {16, 32, 96}") {
    assert(Solution.numbersBetweenSets(Set(2, 4), Set(16, 32, 96)) === 3)
  }

  test("numbers between {2, 3, 4} {16, 32, 96}") {
    assert(Solution.numbersBetweenSets(Set(2, 3, 4), Set(16, 32, 96)) === 0)
  }

  test("numbers between {2, 3, 4} {48, 96, 288}") {
    assert(Solution.numbersBetweenSets(Set(2, 3, 4), Set(48, 96, 288)) === 3)
  }

  test("numbers between {2, 4} {48, 96, 288}") {
    assert(Solution.numbersBetweenSets(Set(2, 4), Set(48, 96, 288)) === 4)
  }

  test("numbers between {2, 3, 4} {144, 288, 864}") {
    assert(Solution.numbersBetweenSets(Set(2, 3, 4), Set(144, 288, 864)) === 4)
  }

  test("numbers between {3, 4, 6} {24}") {
    assert(Solution.numbersBetweenSets(Set(3, 4, 6), Set(24)) === 2)
  }

  test("numbers between {4} {4}") {
    assert(Solution.numbersBetweenSets(Set(4), Set(4)) === 1)
  }

  test("numbers between {4} {7}") {
    assert(Solution.numbersBetweenSets(Set(4), Set(7)) === 0)
  }

  test("numbers between {1} {8}") {
    assert(Solution.numbersBetweenSets(Set(1), Set(8)) === 4)
  }

  test("numbers between {2, 7} {7, 21}") {
    assert(Solution.numbersBetweenSets(Set(2, 7), Set(7, 21)) === 0)
  }
}
