package net.projecteuler.kurtsu

import org.scalatest.funsuite.AnyFunSuite
import Util._

class UtilTest extends AnyFunSuite {
  test("pow") {
    assert(1 == pow(1, 0))
    assert(1 == pow(2, 0))
    assert(1 == pow(10, 0))
    assert(2 == pow(2, 1))
    assert(5 == pow(5, 1))
    (1 to 32).foreach(e => assert(BigInt(2).pow(e).toLong == pow(2, e)))
    (1 to 15).foreach(e => assert(BigInt(3).pow(e).toLong == pow(3, e)))
    (1 to 5).foreach(e => assert(BigInt(641).pow(e).toLong == pow(641, e)))
  }

  test("phi") {
    val phiVals = 0 :: List(1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8,
      12, 10, 22, 8, 20, 12, 18, 12, 28, 8, 30, 16, 20, 16, 24, 12, 36, 18, 24, 16,
      40, 12, 42, 20, 24, 22, 46, 16, 42, 20, 32, 24, 52, 18, 40, 24, 36, 28, 58, 16,
      60, 30, 36, 32, 48, 20, 66, 32, 44, 24, 70, 24, 72, 36, 40, 36, 60, 24, 78, 32,
      54, 40, 82, 24, 64, 42, 56, 40, 88, 24, 72, 44, 60, 46, 72, 32, 96, 42, 60, 40)
    (1 to 100).foreach(n => assert(phi(n) == phiVals(n)))
    assert(phi(89 * 90) == phi(89) * phi(90))
    assert(phi(16 * 27) == phi(16) * phi(27))
  }
}
