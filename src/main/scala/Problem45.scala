package net.projecteuler.kurtsu

object Problem45 extends Solution {
  override val problemNum: Int = 45
  override val difficulty: Int = 5

  override def solution(): String =
    val n = LazyList.from(1 + 165)
      .map(_.toLong)
      .find(n => {
        val root = math.sqrt(12 * n*n - 4 * n + 1)
        math.ceil(root) == math.floor(root) && root.longValue % 4 == 3
      }).get

    ( n * (3 * n - 1) / 2 ).toString
}
