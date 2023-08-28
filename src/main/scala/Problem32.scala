package net.projecteuler.kurtsu

object Problem32 extends Solution {
  override val problemNum: Int = 32
  override val difficulty: Int = 5

  override def solution(): String =
    // x * yyyy = zzzz
    (listAllProducts(1, 4) ++
      // xx * yyy = zzzz
      listAllProducts(2, 3))
      .distinct
      .sum
      .toString

  final def isNPandigital(n: Int)(s: String): Boolean =
    s.length == n &&
      s.sorted == (1 to n).map(_.toString).reduce(_+_)

  private def listAllProducts(xDigit: Int, yDigit: Int): List[Int] = 
    val isNinePandigital = isNPandigital(9)
    val seqToInt = (s: Seq[Int]) => s.map(_.toString).reduce(_+_).toInt

    (1 to 9).combinations(xDigit + yDigit)
      .flatMap(_.permutations)
      .flatMap(perm =>
        val x = seqToInt(perm.take(xDigit))
        val y = seqToInt(perm.takeRight(yDigit))
        val z = x * y
        if isNinePandigital(List(x, y ,z).map(_.toString).reduce(_+_)) then
          Some(z)
        else
          None
      ).toList
}

