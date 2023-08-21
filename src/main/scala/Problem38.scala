package net.projecteuler.kurtsu

import Problem32.isNPandigital

object Problem38 extends Solution {
  override val problemNum: Int = 38
  override val difficulty: Int = 5

  override def solution(): String =
    (
      // n = 2, 4 + 5
      maxNinePandigital(2, 10000 / 2 to 9999) ::
      // n = 3, 3 + 3 + 3
      maxNinePandigital(3, 100 to 1000 / 3) ::
      // n = 4, 2 + 2 + 2 + 3
      maxNinePandigital(4, 1000 / 4 to 999) ::
      // n >= 5, 1 + ...
      (5 to 9).map(n => maxNinePandigital(n, 1 to 9)).toList
      // n = 9, 123456789
    ).max

  private def maxNinePandigital(n: Int, mRange: IndexedSeq[Int]): String =
    val isNinePandigital = isNPandigital(9)

    val allResults = mRange
      .map(a =>
        (1 to n).map(m => a * m)
          .map(_.toString)
          .reduce(_ + _)
      )
      .filter(isNinePandigital)

    allResults match
      case IndexedSeq() => ""
      case _ => allResults.max
}
