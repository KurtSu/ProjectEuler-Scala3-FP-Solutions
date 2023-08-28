package net.projecteuler.kurtsu

import scala.annotation.tailrec
import Util.isPrime

object Problem50 extends Solution {
  override val problemNum: Int = 50
  override val difficulty: Int = 5

  override def solution(): String =
    val sumUpperBound = 1_000_000L
    val primes = 2L :: (3L until sumUpperBound by 2).filter(isPrime).toList
    val maxSeqLength = findMaxSeqLength(primes, sumUpperBound)
    assert(41 == findLongestSeqSum(primes, 100, maxSeqLength))
    assert(953 == findLongestSeqSum(primes, 1000, maxSeqLength))
    findLongestSeqSum(primes, sumUpperBound, maxSeqLength).toString

  /**
   * not the most elegant code I've ever written I hate to admit.
   */
  private def findLongestSeqSum(primes: List[Long], sumUpperBound: Long, maxSeqLength: Int): Long =
    val length = (maxSeqLength to 1 by -1)
      .find(l =>
        // equivalent to if l % 2 == 0 then evenLengthSumPrime() else oddLengthSumPrime()
        (l % 2 == 0 && evenLengthSumPrime(primes, l, sumUpperBound).isDefined) ||
          oddLengthSumPrime(primes.tail, l, sumUpperBound).isDefined
      )
      .get

    if length % 2 == 0 then evenLengthSumPrime(primes, length, sumUpperBound).get
    else oddLengthSumPrime(primes.tail, length, sumUpperBound).get

  private def findMaxSeqLength(primes: List[Long], sumUpperBound: Long): Int =
    LazyList.from(1)
      .find(l => primes.take(l).sum >= sumUpperBound)
      .get

  private def evenLengthSumPrime(primes: List[Long], length: Int, sumUpperBound: Long): Option[Long] =
    val sum = primes.take(length).sum
    if sum < sumUpperBound && isPrime(sum) then Some(sum)
    else None

  /** `primes` should start with 3
   *  FIXME this code is buggy, as it should check sum in REVERSE order, and therefore
   *  should not quit early if `sum >= sumUpperBound`
   *  most obviously, `oddLengthSumPrime(primes, 1, 10)` will return Some(3), instead of Some(7)
   */
  @tailrec
  private def oddLengthSumPrime(primes: List[Long], length: Int, sumUpperBound: Long): Option[Long] =
    // reaching the end of the primes list.
    if primes.length < length then None
    else
      val sum = primes.take(length).sum
      // no need to consider further as the current sum is already too large
      if sum >= sumUpperBound then None
      else
        // find one prime, immediately quit
        if isPrime(sum) then Some(sum)
        // keep finding
        else oddLengthSumPrime(primes.tail, length, sumUpperBound)
}
