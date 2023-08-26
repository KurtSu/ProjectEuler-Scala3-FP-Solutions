package net.projecteuler.kurtsu

import Util.comb

object Problem53 extends Solution {
  override val problemNum: Int = 53
  override val difficulty: Int = 5

  override def solution(): String = {
    // todo
    (1 to 100).map(n =>
      n - 2 * (0 to n / 2).takeWhile(r =>
        comb(n, r) <= 1_000_000
      ).length
    ).sum.toString
  }

  def main(args: Array[String]): Unit = {
    println(solution())
  }
}
