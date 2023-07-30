package net.projecteuler.kurtsu

import scala.math.{ceil, floor}
import Util.gcd

object Problem73 extends Solution {
  override def problemNum: Int = 73
  override def difficulty: Int = 15

  override final def solution(): String = {
    assert(3 == rpfInRange(8))
    rpfInRange(12000).toString
  }

  /**
   * given `denominator`, find the number of reduced proper fractions with numerators
   * within range `(lower to upper)`
   * @param denominator
   * @param lower
   * @param upper
   * @return
   */
  private def rpfInBetween(denominator: Int, lower: Int, upper: Int): Int = {
    (lower to upper).count(numerator => 1 == gcd(denominator, numerator))
  }

  /**
   * Given lower bound 1/3 and upper bound 1/2, count the rpf in range `4 to d` with in (1/3, 1/2)
   * @param d
   * @return
   */
  private def rpfInRange(d: Int): Int = {
    val calcLower = (denominator: Int) => ceil(denominator / 3.0).toInt
    val calcUpper = (denominator: Int) => floor(denominator / 2.0).toInt
    (4 to d)
      .map(denominator => rpfInBetween(denominator, calcLower(denominator), calcUpper(denominator)))
      .sum
  }
}
