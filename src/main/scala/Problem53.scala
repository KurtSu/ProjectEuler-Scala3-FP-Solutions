package net.projecteuler.kurtsu

import Util.comb

object Problem53 extends Solution {
  override val problemNum: Int = 53
  override val difficulty: Int = 5

  override def solution(): String =
    // lower bound given by problem
    (23 to 100)
      .map(n =>
        (0 to n / 2).find(r => comb(n, r) > 1_000_000) match
          case Some(r) => n + 1 - 2 * r
          case None => 0
      )
      .sum
      .toString
}
