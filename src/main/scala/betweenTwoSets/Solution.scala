package betweenTwoSets

import scala.annotation.tailrec

/**
  * Created by horiaradu on 31/10/2016.
  */
object Solution {
  def primeFactors(n: Int): Map[Int, Int] = {
    val sqrtOfN = math.sqrt(n).ceil

    @tailrec
    def checkFactor(factor: Int, power: Int, factorToThePower: Int, n: Int): (Int, Int) =
      if (n % factorToThePower == 0) checkFactor(factor, power + 1, factorToThePower * factor, n)
      else (factorToThePower / factor, power - 1)

    @tailrec
    def buildPrimeFactors(factor: Int, n: Int, acc: Map[Int, Int]): Map[Int, Int] =
      if (factor > sqrtOfN) {
        if (acc == Map(1 -> 1)) acc + (n -> 1)
        else acc
      } else {
        val (factorToThePower, power) = checkFactor(factor, 1, factor, n)
        if (power == 0) buildPrimeFactors(factor + 1, n, acc)
        else buildPrimeFactors(factor + 1, n / factorToThePower, acc + (factor -> power))
      }

    buildPrimeFactors(2, n, Map(1 -> 1))
  }

  /**
    * greatest common divisor
    */
  def gcd(xs: Set[Int]): Map[Int, Int] = {
    def mergeFactorMaps(resultFactors: Map[Int, Int], xFactors: Map[Int, Int]): Map[Int, Int] = {
      xFactors.foldLeft(Map[Int, Int]()) {
        case (result, (factor, power)) =>
          if (resultFactors.contains(factor)) result + (factor -> math.min(power, resultFactors(factor)))
          else result
      }
    }

    xs.map(primeFactors)
      .reduce(mergeFactorMaps)
  }

  /**
    * lowest common multiple
    */
  def lcm(xs: Set[Int]): Map[Int, Int] = {
    def mergeFactorMaps(resultFactors: Map[Int, Int], xFactors: Map[Int, Int]): Map[Int, Int] = {
      xFactors.foldLeft(resultFactors) {
        case (result, (factor, power)) =>
          if (resultFactors.contains(factor)) result + (factor -> math.max(power, resultFactors(factor)))
          else result + (factor -> power)
      }
    }

    xs.map(primeFactors)
      .reduce(mergeFactorMaps)
  }

  def numbersBetweenSets(as: Set[Int], bs: Set[Int]): Int = {
    val lcmOfAs = lcm(as)
    val gcdOfBs = gcd(bs)

    if (lcmOfAs.keys.forall(factor => gcdOfBs.contains(factor))) {
      lcmOfAs
        .foldLeft(gcdOfBs) {
          case (result, (factor, power)) => result + (factor -> (result(factor) - power))
        }
        .values
        .sum + 1
    } else 0
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val n = sc.nextInt()
    val m = sc.nextInt()

    var as = Set[Int]()
    for (i <- 0 until n) {
      as += sc.nextInt()
    }
    var bs = Set[Int]()
    for (i <- 0 until m) {
      bs += sc.nextInt()
    }

    println(numbersBetweenSets(as, bs))
  }
}
