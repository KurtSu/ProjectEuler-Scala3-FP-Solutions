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

  /** Least common multiple */
  @tailrec
  final def lcm(a: Long, b: Long): Long =
    if a * b < 0 then lcm(a, -b)
    else a * b / gcd(a, b)

  // TODO obviously, rewrite it.
  @tailrec
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

  final def leastPrimeFactor(n: Long): Long =
    if n % 2 == 0 then 2
    else
      (3L to sqrt(n).longValue by 2)
        .find(p => n % p == 0) match
        case Some(p) => p
        case None => n

  @tailrec
  final def isPrime(n: Long): Boolean =
    if n < 0 then isPrime(-n)
    else if n < 2 then false
    else leastPrimeFactor(n) == n

  /**
   * Perform standard factorization for `n`
   * @param n n
   * @return A list of prime factor of `n`, in reverse order.
   */
  @tailrec
  final def factorization(n: Long): List[Long] =
    @tailrec
    def factorizationHelper(n: Long, acc: List[Long]): List[Long] =
      if n <= 1 then acc
      else
        val lpf = leastPrimeFactor(n)
        factorizationHelper(n / lpf, lpf :: acc)

    if n < 0 then factorization(-n)
    else factorizationHelper(n, Nil)

  final def factorizationUniquePrimeAndExponent(n: Long): List[(Long, Int)] =
    @tailrec
    def uniquePrimeHelper(factors: List[Long], acc: List[(Long, Int)]): List[(Long, Int)] =
      factors match
        case Nil => acc
        case factors =>
          val primeRepeated = factors.takeWhile(p => p == factors.head)
          val exp = primeRepeated.length
          uniquePrimeHelper(factors.drop(exp), (factors.head, exp) :: acc)

    uniquePrimeHelper(factorization(n), Nil)

  final def phi(n: Long): Long =
    @tailrec
    def processTupleList(uniqueFactors: List[(Long, Int)], acc: Long): Long =
      uniqueFactors match
        case Nil => acc
        case (p, e) :: others =>
          processTupleList(others, acc * pow(p, e-1) * (p-1))

    processTupleList(factorizationUniquePrimeAndExponent(n), 1)

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
