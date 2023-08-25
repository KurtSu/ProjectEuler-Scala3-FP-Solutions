package net.projecteuler.kurtsu

import Util.factorization

object Problem3 extends Solution {
  override val problemNum: Int = 3
  override val difficulty: Int = 5

  override final def solution(): String =
    factorization(600851475143L).max.toString
}
