package net.projecteuler.kurtsu

import scala.math.{log10, floor}

object Problem63 extends Solution {
  override val problemNum: Int = 63
  override val difficulty: Int = 5

  override def solution(): String =
    (1 to 9).map(b =>
      floor(1.0 / (1-log10(b))).toInt
    ).sum.toString
}
