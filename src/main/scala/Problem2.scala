package net.projecteuler.kurtsu

import math.{sqrt, round, log, pow}

object Problem2 extends Solution {
  override val problemNum: Int = 2
  override val difficulty: Int = 5

  override final def solution(): String =
    val upperBound = 4e6
    val radical5 = sqrt(5)
    val phi3 = 2 + sqrt(5)
    val n = (log(upperBound * radical5) / log(phi3)).intValue
    sumOfEvenFib(n).toString

  private def sumOfEvenFib(n: Int): Long =
    val radical5 = sqrt(5)
    val phi3 = 2 + sqrt(5)
    (round((1 + phi3) / radical5 * pow(phi3, n)).longValue - 2) / 4

  def main(args: Array[String]): Unit = {
    println(solution())
  }
}
