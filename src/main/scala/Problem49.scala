package net.projecteuler.kurtsu

import Util.isPrime

object Problem49 extends Solution {
  override val problemNum: Int = 49
  override val difficulty: Int = 5

  override def solution(): String =
    val primes = (1001 until 10000 by 2).filter(isPrime)
    (6 until (10000-1000) / 2 by 6)
      .flatMap(d =>
        primes
          .filter(p1 =>
            val p2 = p1 + d
            val p3 = p1 + 2 * d
            val p1String = p1.toString.sorted
            p1String == p2.toString.sorted && p1String == p3.toString.sorted &&
              primes.contains(p2) && primes.contains(p3)
          )
          .map(p => List(p, p + d, p + 2 * d).map(_.toString).reduce(_+_))
      )
      .filter(_ != "148748178147")
      .head
}
