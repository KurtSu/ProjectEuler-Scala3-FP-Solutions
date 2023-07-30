package net.projecteuler.kurtsu

import scala.annotation.tailrec
import Constants._
/**
 * a collection of utilities
 */
object Util {
  @tailrec
  final def gcd(a: Int, b: Int): Int = {
    if (a < b) gcd(b, a)
    else {
      b match
        case 0 => a
        case _ => gcd(b, a % b)
    }
  }
}
