package net.projecteuler.kurtsu

import scala.annotation.tailrec
import scala.math.pow

object Problem40 extends Solution {
  override val problemNum: Int = 40

  override def solution(): String = {
    assert(1 == d(12))

    (0 to 6).map(pow(10, _).toInt)
      .map(d)
      .product
      .toString
  }


  /**
   * find the largest position (1-base) in the sequence for all `n`-digit #s
   * @param n the number of digits. e.g. given number 999, then n = 3, since it's a 3-digit #
   * @param accumulator
   * @return the largest position (1-base)
   */
  @tailrec
  private def largestNDigitNumPos(n: Int, accumulator: Int = 0): Int = {
    n match
      case 0 => accumulator
      case n => largestNDigitNumPos(n - 1, accumulator + 9 * pow(10, n - 1).toInt * n)
  }

  /**
   * find the digit of `theNumber` in the sequence given `pos`. e.g. given pos = 12, then 2
   * @param pos the position (1-base) in the sequence
   * @param digit tailrec accumulator
   * @return the digit of `theNumber`
   */
  @tailrec
  private def numDigitAtPos(pos: Int, digit: Int = 0): Int = {
    val maxPos = largestNDigitNumPos(digit)
    if (pos <= maxPos) digit
    else numDigitAtPos(pos, digit + 1)
  }

  /**
   * The d_n defined in the problem
   * @param pos 1-based position
   * @return d of pos
   */
  private def d(pos: Int): Int = {
    // there is a number at pos, let's call it `theNumber`.
    // e.g., given pos = 12, then `theNumber` = 11

    // the length of theNumber
    val theNumberLen = numDigitAtPos(pos)
    // the smallest `theNumberLen`-digit #. e.g. the smallest 2-digit # will be 10
    val smallestOfLen = pow(10, theNumberLen - 1).toInt
    // the starting position of `smallestOfLen`
    val baseStartPos = largestNDigitNumPos(theNumberLen - 1) + 1
    // how far away is `theNumber` from `smallestOfLen`
    val posIdx = (pos - baseStartPos) / theNumberLen
    // which digit is `pos` at `theNumber`, 0-base index
    val posOffset = (pos - baseStartPos) % theNumberLen

    val theNumber = smallestOfLen + posIdx
    theNumber.toString()(posOffset).toInt - '0'
  }
}
