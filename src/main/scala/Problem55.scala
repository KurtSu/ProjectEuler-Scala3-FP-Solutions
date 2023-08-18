package net.projecteuler.kurtsu

import scala.annotation.tailrec

object Problem55 extends Solution {
  override val problemNum: Int = 55
  override val difficulty: Int = 5

  override def solution(): String =
    assert(isLychrel(196))
    assert(!isLychrel(47))
    assert(!isLychrel(349))
    assert(isLychrel(4994))
    (1 until 10000).count(isLychrel(_)).toString

  private def isPalindromic(n: BigInt): Boolean =
    val s = n.toString
    s.reverse == s

  private def nextNum(n: BigInt): BigInt =
    n + BigInt(n.toString.reverse)

  @tailrec
  private def isLychrel(n: BigInt, iteration: Int=1): Boolean =
    if iteration > 50 then true
    else
      val nPrime = nextNum(n)
      if isPalindromic(nPrime) then false
      else isLychrel(nPrime, 1 + iteration)
}
