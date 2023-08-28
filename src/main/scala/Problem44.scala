package net.projecteuler.kurtsu

import Util.quadraticEqSolver

object Problem44 extends Solution {
  override val problemNum: Int = 44
  override val difficulty: Int = 5

  override def solution(): String =
    LazyList.from(1)
      .map(_.toLong)
      .find(b =>
        (1L to b-1).exists(a =>
          val pa = p(a)
          val pb = p(b)
          val pc = pb + pa
          val pd = pa + pc
          val pdAlt = pb + pc
          isPentagonal(pc) && (isPentagonal(pd) || isPentagonal(pdAlt))
        )
      )
      .map(p)
      .get
      .toString

  private def p(n: Long): Long =
    n * (3 * n - 1) / 2

  private def isPentagonal(n: Long): Boolean =
    quadraticEqSolver(1.5, -0.5, -n) match
      case (Some(_), Some(x)) => x % 1 == 0
      case _ => false
}
