package net.projecteuler.kurtsu

import scala.annotation.tailrec
import scala.math.sqrt

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
      case _ if b < 0 => gcd(a, -b)
      // eventually a >= b >= 0
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

  final def quadraticEqSolver(a: Double, b: Double, c: Double): (Option[Double], Option[Double]) =
    assert(a != 0)
    val discriminant = b * b - 4 * a * c
    if discriminant > 0 then
      (Some( (-b - sqrt(discriminant)) / a / 2.0 ),
       Some( (-b + sqrt(discriminant)) / a / 2.0 ))

    else if discriminant == 0 then
      (Some ( -b / a / 2.0 ), None)

    else
      (None, None)
  
  final def pow(b: Long, e: Long): Long =
    @tailrec
    def powHelper(b: Long, e: Long, acc: Long): Long =
      if e == 0 then acc
      else if e == 1 then acc * b
      else if e % 2 == 0 then powHelper(b * b, e / 2, acc)
      else powHelper(b * b, e / 2, acc * b)

    powHelper(b, e, 1)

  final def digitalSum(s: String): Long =
    @tailrec
    def digitalSumHelper(s: String, acc: Long): Long =
      s match
        case "" => acc
        case _ => digitalSumHelper(s.tail, s.head-'0' + acc)

    s match
      case _ if s.exists(!_.isDigit) => -1
      case _ => digitalSumHelper(s, 0L)
}
