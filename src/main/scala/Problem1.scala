package net.projecteuler.kurtsu

object Problem1 extends Solution {
  override val problemNum: Int = 1
  override val difficulty: Int = 5

  override final def solution(): String =
    (1 until 1000).filter(n => n % 3 == 0 || n % 5 == 0).sum.toString
}
