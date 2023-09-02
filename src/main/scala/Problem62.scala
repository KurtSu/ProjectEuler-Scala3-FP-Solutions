package net.projecteuler.kurtsu

import scala.math.{pow, floor, ceil}

object Problem62 extends Solution {
  override val problemNum: Int = 62
  override val difficulty: Int = 15

  override final def solution(): String =
    val numOfPerm = 5
    val cubeRoot = pow(_, 1.0 / 3.0)

    LazyList.from(7)
      .map(n => pow(10.0, n))
      .map(low => ceil(cubeRoot(low)).toLong to floor(cubeRoot(10 * low - 1)).toLong)
      .map(roots => roots.map(root => root * root * root))
      .map(cubesOfSameDigits =>
        cubesOfSameDigits
          // group by permutation
          .groupBy(n => n.toString.sorted)
          // pick all permutations has 5 elements.
          .filter((_, cubesOfPerm) => numOfPerm == cubesOfPerm.length)
          // get the lists and flatten them
          .values
          .flatten match
            case Nil => None
            case it => Some(it.min)
      )
      .collectFirst { case Some(n) => n }
      .get
      .toString
}
