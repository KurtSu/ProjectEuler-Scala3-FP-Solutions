package net.projecteuler.kurtsu

import scala.annotation.tailrec

object Problem89 extends Solution {
  override val problemNum: Int = 89
  override val difficulty: Int = 20

  override final def solution(): String =
    val fileURL = "https://projecteuler.net/resources/documents/0089_roman.txt"
    val buffer = scala.io.Source.fromURL(fileURL)
    val ret = try buffer.getLines
      .map(r => (r, aToMinR(rToA(r))))
      .map{ case(ori, min) => ori.length - min.length }
      .sum
    finally buffer.close
    ret.toString

  private val rToAMap: Map[Char, Int] = Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

  private val aToRMap: Map[Int, String] = rToAMap.map({ case (c, i) => (i, c.toString) }) ++
    Map(
      4 -> "IV",
      9 -> "IX",
      40 -> "XL",
      90 -> "XC",
      400 -> "CD",
      900 -> "CM"
    )

  /**
   * convert a well-formed Roman number to an Arabic number
   * @param r a well-formed (not necessarily minimal) Roman number
   * @return the decoded Arabic number
   */
  final def rToA(r: String): Int = 
    @tailrec
    def rToAHelper(r: String, accArab: Int): Int = 
      if (r.isEmpty) accArab
      else if (r.length == 1) rToAHelper(r.tail, rToAMap(r.head) + accArab)
      else 
        val firstArab = rToAMap(r.head)
        val secondArab = rToAMap(r(1))
        if (firstArab < secondArab)
          // subtractive rule
          rToAHelper(r.drop(2), (secondArab - firstArab) + accArab)
        else
          rToAHelper(r.tail, firstArab + accArab)
      

    rToAHelper(r, 0)

  /**
   * convert an Arabic number to a Roman number in minimal form
   * @param n an Arabic number
   * @return encoded Roman number in minimal form
   */
  final def aToMinR(n: Int): String = 
    @tailrec
    def aToMinRHelper(n: Int, accRome: String, denominationsRemain: List[Int]): String = 
      n match
        case 0 => accRome
        case _ =>
          val largestDenominationArab = denominationsRemain.head
          val numberOfLetters = n / largestDenominationArab
          aToMinRHelper(
            n - largestDenominationArab * numberOfLetters,
            accRome + aToRMap(largestDenominationArab) * numberOfLetters,
            denominationsRemain.tail
          )

    // the 3rd arg is like List(1000, 900, 500, 400, ..., 5, 4, 1)
    aToMinRHelper(n, "", aToRMap.keys.toList.sortBy(-_))
}
