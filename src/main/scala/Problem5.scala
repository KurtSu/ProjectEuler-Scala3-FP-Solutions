package net.projecteuler.kurtsu

import Util.lcm

object Problem5 extends Solution {
  override val problemNum: Int = 5
  override val difficulty: Int = 5

  override final def solution(): String =
    (1L to 20L).reduce(lcm).toString

}
