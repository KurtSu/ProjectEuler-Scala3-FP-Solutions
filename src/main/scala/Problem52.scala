package net.projecteuler.kurtsu

object Problem52 extends Solution {
  override val problemNum: Int = 52
  override val difficulty: Int = 5

  override def solution(): String =
    LazyList.from(100_000)
      .find(x =>
        val xString = x.toString.sorted
        (2 to 6).forall(m => (m * x).toString.sorted == xString)
      )
      .get
      .toString
}
