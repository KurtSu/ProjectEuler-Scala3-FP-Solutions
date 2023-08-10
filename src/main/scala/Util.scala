package net.projecteuler.kurtsu

import scala.annotation.tailrec

import Constants._

/**
 * a collection of utilities
 */
object Util {
  /**
   * Greatest common divisor, Euclid's algorithm.
   */
  @tailrec
  final def gcd(a: Long, b: Long): Long = {
    b match
      case _ if a < b => gcd(b, a)
      case 0 => a
      case _ => gcd(b, a % b)
  }

  // TODO obviously, rewrite it.
  final def comb(n: Int, r: Int): BigInt = {
    if (r > n / 2) comb(n, n-r)
    else {
      var acc: BigInt = 1
      (0 until r).foreach(i => {
        acc = acc * (n - i) / (1 + i)
      })
      acc
    }
  }

  /**
   * TODO rewrite it in a functional way
   * Check if a number is prime number or not.
   * 12/19/2019
   *
   * @param n the number.
   * @return true for prime number, false otherwise.
   */
  final def isPrime(n: Long): Boolean = {
    if (n < 0)
      isPrime(-n)

    else {
      if (n % 2 == 0 & n > 2)
        false

      else if (n == 1 || n == 0)
        false

      else {
        var factor: Int = 3

        while (factor <= math.sqrt(n)) {
          if (n % factor == 0)
            return false
          factor += 2
        }

        true
      }
    }
  }
}
