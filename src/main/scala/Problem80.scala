package net.projecteuler.kurtsu

import scala.math.sqrt
import Util.digitalSum

object Problem80 extends Solution {
  override val problemNum: Int = 80
  override val difficulty: Int = 20

  override def solution(): String =
    // compute at least 100 digits after decimal to avoid error caused by rounding
    val mc = java.math.MathContext(102)

    (1 to 100)
      .filter(n => sqrt(n) != sqrt(n).intValue)
      .map(n => java.math.BigDecimal(n).sqrt(mc).toString)
      .map(s => s.replace(".", ""))
      .map(s => digitalSum(s.take(100)))
      .sum
      .toString
}
