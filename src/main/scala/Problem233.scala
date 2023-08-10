package net.projecteuler.kurtsu

import scala.math.{pow, ceil}

import Util.isPrime

object Problem233 extends Solution {
  override def problemNum: Int = 233

  override def difficulty: Int = 70

  override def solution(): String = {
    val maxN: Long = 1e11.toLong
    val primeUpperBound: Long = ceil(maxN / pow(5, 3) / pow(13, 2)).toLong
    // all primes in the form of 4k+1
    val primeList: List[Long] = (5L to primeUpperBound by 4L)
      .filter(isPrime).toList
    val exponentsLists = List(
      List(52),
      List(17, 1),
      List(10, 2),
      List(7, 3),
      List(3, 2, 1)
    )
    val s1 = System.currentTimeMillis()
    val baseNumbers = exponentsLists.flatMap(exponents =>
      findAllPrimeProd(primeList, exponents, maxN)
    )
    val e1 = System.currentTimeMillis()
    println(s"baseNumber costed ${e1-s1} ms")
    val ret = findSum(baseNumbers, maxN, primeList).toString
    val e2 = System.currentTimeMillis()
    println(s"findSum costed ${e2-e1} ms")
    ret
  }

  private def findAllPrimeProd(
                                primes: List[Long],
                                exponents: List[Int],
                                maxN: Long,
                                prev: Long = 1L,
                              ): List[Long] = {
    exponents match
      case Nil =>
        List() // should never happen

      case e :: Nil =>
        primes
          .filter(_ <= ceil(pow(maxN / prev.toDouble, 1.0 / e)).toLong)
          .map(p => prev * pow(p, e).toLong)

      case e :: tail =>
        primes
          .filter(_ <= ceil(pow(maxN / prev.toDouble, 1.0 / e)).toLong)
          .flatMap(p =>
            findAllPrimeProd(
              primes.filter(_ != p),
              tail,
              maxN,
              prev * pow(p, e).toLong
          ))
  }

  private def findSum(bases: List[Long], maxN: Long, primes: List[Long]): Long = {
    val multiplyVector = bases.map(b =>
      (1L to maxN / b).filter(m => !primes.contains(m)).sum
    )

    bases.zip(multiplyVector)
      .map { case(b, m) => b * m }
      .sum
  }

  def main(args: Array[String]): Unit = {
    val s = System.currentTimeMillis()
    solution()
    val e = System.currentTimeMillis()
    println(s"time past: ${e-s} ms")
  }
}
