package net.projecteuler.kurtsu

import Util.isPrime

object Problem7 extends Solution {
  override val problemNum: Int = 7
  override val difficulty: Int = 5

  override final def solution(): String =
    LazyList.from(3, 2)
      .map(_.toLong)
      .filter(isPrime)
      .take(10001 - 1)
      .last
      .toString
}
