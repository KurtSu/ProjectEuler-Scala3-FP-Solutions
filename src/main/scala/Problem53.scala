package net.projecteuler.kurtsu

import Util.comb

object Problem53 extends Solution {
  override val problemNum: Int = 53
  override val difficulty: Int = 5

  override def solution(): String = {
    (1 to 100).map(n =>
      (1 to n).count(r =>
        comb(n, r) > 1_000_000
      )
    ).sum.toString
  }
}
