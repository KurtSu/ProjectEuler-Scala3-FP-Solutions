package net.projecteuler.kurtsu

import Util.isPrime

object Problem10 extends Solution {
  override val problemNum: Int = 10
  override val difficulty: Int = 5

  override final def solution(): String =
    (1L until 2_000_000L)
      .filter(isPrime)
      .sum
      .toString
}
