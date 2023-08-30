package net.projecteuler.kurtsu

import math.sqrt
import Util.isPrime

object Problem70 extends Solution {
  override val problemNum: Int = 70
  override val difficulty: Int = 20

  override def solution(): String =
    val upperBound = 10_000_000L
    val rootUpperBound = sqrt(upperBound).toLong

    // It's guaranteed that there exists n=p1p2, phi(n) is a perm of n, e.g., n = 87109 = 11 * 7919.
    // so we only care about n = p1p2
    val primePairs = for {
      // this lower and upper bounds are empirical
      p1 <- (rootUpperBound / 4 * 2 + 1 to rootUpperBound     by 2).filter(isPrime)
      p2 <- (rootUpperBound / 2 * 2 + 1 to rootUpperBound * 2 by 2).filter(isPrime)
    } yield (p1, p2)

    primePairs
      .flatMap { case(p1, p2) =>
        val n = p1 * p2
        val phin = phiOfProductOfTwoPrimes(p1, p2)
        if n < upperBound && phin.toString.sorted == n.toString.sorted then Some((n, phin))
        else None
      }
      .min(Ordering.by[(Long, Long), Double] { case (n, phin) => n.toDouble / phin } )
      ._1
      .toString

  private def phiOfProductOfTwoPrimes(p1: Long, p2: Long): Long =
    (p1 - 1) * (p2 - 1)
}
