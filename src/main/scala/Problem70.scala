package net.projecteuler.kurtsu

import math.sqrt
import Util.{isPrime, phi}

object Problem70 extends Solution {
  override val problemNum: Int = 70
  override val difficulty: Int = 20

  override def solution(): String =
    val upperBound = 10_000_000L
    val rootUpperBound = sqrt(upperBound).toLong
    // this lower and upper bounds are empirical
    val primes = (rootUpperBound / 2 to rootUpperBound * 2).filter(isPrime)

    // There is no guarantee (that I can justify) that when is product of two primes,
    // it will yield a non-empty list, luckily it did.
    primes
      .combinations(2)
      .map(_.product)
      .filter(_ <= upperBound)
      .flatMap(n =>
        val phin = phi(n)
        if phin.toString.sorted == n.toString.sorted then Some((n, phin))
        else None
      ).toList
      .min(Ordering.by[(Long, Long), Double] { case (n, phin) => n.toDouble / phin } )
      ._1
      .toString
}
