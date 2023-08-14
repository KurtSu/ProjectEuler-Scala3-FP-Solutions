package net.projecteuler.kurtsu

import scala.math.{log10, floor}

object Problem63 extends Solution {
  override val problemNum: Int = 63
  override val difficulty: Int = 5

  override def solution(): String =
    (1 to floor(1 / (1-log10(9))).toInt)
      .map(d =>
        (1 to 9).count(b =>
          log10(b) >= (d-1).toDouble / d.toDouble
        )
      ).sum.toString
}
