package net.projecteuler.kurtsu

import Util.phi

object Problem69 extends Solution {
  override val problemNum: Int = 69
  override val difficulty: Int = 10

  override def solution(): String =
    (1L to 1_000_000L)
      .map(n => (n, n.toDouble / phi(n)))
      .max(Ordering.by[(Long, Double), Double](_._2))
      ._1
      .toString
}
