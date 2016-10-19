package candies

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by horiaradu on 16/10/2016.
  */
@RunWith(classOf[JUnitRunner])
class CandiesSuite extends FunSuite {
  test("(1 2 2) should require 3 candies") {
    assert(Solution.candies(List(1, 2, 2)) === 4)
  }

  test("(3 2 1) should require 6 candies") {
    assert(Solution.candies(List(3, 2, 1)) === 6)
  }

  test("(1 1 1) should require 3 candies") {
    assert(Solution.candies(List(1, 1, 1)) === 3)
  }

  test("(2, 4, 2, 6, 1, 7, 8, 9, 2, 1) should require 19 candies") {
    assert(Solution.candies(List(2, 4, 2, 6, 1, 7, 8, 9, 2, 1)) === 19)
//    2, 4, 2, 6, 1, 7, 8, 9, 2, 1
//    1, 2, 1, 2, 1, 2, 3, 4, 2, 1
  }
}
