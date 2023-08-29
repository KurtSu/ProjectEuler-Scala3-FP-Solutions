package net.projecteuler.kurtsu

import Util.phi

object Problem69 extends Solution {
  override val problemNum: Int = 69
  override val difficulty: Int = 10

  // TODO write a post and optimize it to 2 * 3 * ... * 17
  override def solution(): String =
    (1L to 1_000_000L)
      .map(n => (n, phi(n)))
      .max(Ordering.by[(Long, Long), Double] { case (n, phin) => n.toDouble / phin } )
      ._1
      .toString
}
