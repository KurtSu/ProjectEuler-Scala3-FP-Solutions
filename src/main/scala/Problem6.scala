package net.projecteuler.kurtsu

object Problem6 extends Solution {
  override val problemNum: Int = 6
  override val difficulty: Int = 5

  override final def solution(): String =
    ( sumOfNaturalNumbers(100) * sumOfNaturalNumbers(100) - sumOfSquares(100) ).toString

  private def sumOfNaturalNumbers(n: Int): Int = n * (n+1) / 2
  private def sumOfSquares(n: Int): Int = n * (n+1) * (2*n+1) / 6
}
