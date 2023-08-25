package net.projecteuler.kurtsu

import Util.phi

object Problem72 extends Solution {
  override val problemNum: Int = 72
  override val difficulty: Int = 20

  override final def solution(): String =
    assert(21 == rpfUnderD(8))
    rpfUnderD(1_000_000).toString

  private def rpfUnderD(d: Long): Long =
    (2L to d).map(phi).sum
}
