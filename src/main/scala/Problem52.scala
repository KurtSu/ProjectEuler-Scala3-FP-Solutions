package net.projecteuler.kurtsu

object Problem52 extends Solution {
  override val problemNum: Int = 52
  override val difficulty: Int = 5

  override def solution(): String =
    LazyList.from(1)
      .find(x =>
        val xString = x.toString.sorted
        (2 * x).toString.sorted == xString &&
          (3 * x).toString.sorted == xString &&
          (4 * x).toString.sorted == xString &&
          (5 * x).toString.sorted == xString &&
          (6 * x).toString.sorted == xString
      )
      .get
      .toString
}
