package net.projecteuler.kurtsu

import scala.annotation.tailrec
import Constants._

/**
 * a collection of utilities
 */
object Util {
  /**
   * Greatest common divisor of `a` and `b`
   * @param a the larger one
   * @param b the smaller one
   * @return
   */
  final def gcd(a: Int, b: Int): Int = {
    @tailrec
    def gcdHelper(a: Int, b: Int): Int = {
      b match
        case 0 => a
        case _ => gcdHelper(b, a % b)
    }

    if (a < b) gcdHelper(b, a)
    else gcdHelper(a, b)
  }
}
