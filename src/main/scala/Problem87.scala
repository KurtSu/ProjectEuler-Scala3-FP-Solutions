package net.projecteuler.kurtsu

import math.pow
import Util.isPrime

object Problem87 extends Solution {
  override val problemNum: Int = 87
  override val difficulty: Int = 20

  override def solution(): String =
    val upperBound = 50_000_000L
    val powerUpperBounds = (2 to 4).map(x => pow(upperBound, 1.0 / x).toLong)
    val primes = 2L +: (3L to powerUpperBounds(0) by 2).filter(isPrime)

    val abcs = for {
      a <- primes.takeWhile(_ <= powerUpperBounds(0))
      b <- primes.takeWhile(_ <= powerUpperBounds(1))
      c <- primes.takeWhile(_ <= powerUpperBounds(2))
    } yield (a, b, c)

    abcs
      .map { case (a, b, c) => pow(a, 2) + pow(b, 3) + pow(c, 4) }
      .distinct
      .count(_ < upperBound)
      .toString
}
