package net.projecteuler.kurtsu

object Problem31 extends Solution {
  override val problemNum: Int = 31
  override val difficulty: Int = 5

  override def solution(): String =
    val total = 200
    val denominations = Array(1, 2, 5, 10, 20, 50, 100, 200)
    coinComb(total, denominations).toString

  /** Dynamic programming */
  private def coinComb(total: Int, denominations: Array[Int]): Int =
    val dp = Array.fill(1 + total)(0)
    // 0 pence counts as 1 combination.
    dp(0) = 1
    for coin <- denominations do
      for pences <- coin to total do
        dp(pences) += dp(pences - coin)
    dp.last
}
