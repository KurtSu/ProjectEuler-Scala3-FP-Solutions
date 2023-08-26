package net.projecteuler.kurtsu

import math.{sqrt, round, abs, ceil}
import Util.{comb, quadraticEqSolver}

object Problem85 extends Solution {
  override val problemNum: Int = 85
  override val difficulty: Int = 15

  override final def solution(): String =
    val target: Double = 2e6
    val nUpperBound = round(getPositiveRoot(quadraticEqSolver(0.5, 0.5, -sqrt(target))))
    (1L to nUpperBound)
      .map(n =>
        val m = round(getPositiveRoot(quadraticEqSolver(0.5, 0.5, -target / comb(n+1, 2))))
        val diff = abs(target - comb(m+1, 2) * comb(n+1, 2)).toLong
        (m * n, diff)
      )
      .min(Ordering.by[(Long, Long), Long](_._2))
      ._1
      .toString

  private def getPositiveRoot(roots: (Option[Double], Option[Double])): Double =
    roots._2.get
}
