package net.projecteuler.kurtsu

import Util.factorizationUniquePrimeAndExponent

object Problem47 extends Solution {
  override val problemNum: Int = 47
  override val difficulty: Int = 5

  override final def solution(): String =
    assert(nConsecutiveUniquePrimeFactors(2, 2).contains(14))
    assert(nConsecutiveUniquePrimeFactors(3, 16).contains(644))
    nConsecutiveUniquePrimeFactors(4, 647).get.toString

  private def nConsecutiveUniquePrimeFactors(n: Int, from: Int): Option[Long] =
    LazyList.from(from)
      .map(_.toLong)
      .map(m => (m, factorizationUniquePrimeAndExponent(m).length))
      .sliding(n)
      .find(l => l.forall { case (_, factors) => factors == n }) match
      case Some(l) => Some(l.head._1)
      case None => None
}
