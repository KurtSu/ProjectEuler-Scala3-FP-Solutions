package net.projecteuler.kurtsu

object Problem90 extends Solution {
  override val problemNum: Int = 90
  override val difficulty: Int = 40

  override final def solution(): String = {
    val allNums = "0123456789"
    val setSize = 6
    val squares = List("01", "04", "06", "16", "25", "36", "46", "64", "81")

    def primitiveFilter(set: String): Boolean =
      squares.forall(square =>
        set.replace("9", "6").contains(square.head) ||
          set.replace("9", "6").contains(square.last)
    )

    val comb1 = allNums.combinations(setSize).filter(primitiveFilter)

    comb1.map(set1 => {
      val comb2 = allNums.combinations(setSize).filter(primitiveFilter)
        .map(_.sorted)

      comb2.count(set2 => {
        if (set1 > set2) false  // to avoid duplication
        else squares.forall(square => {
          val set1NoNine = set1.replace("9", "6")
          val set2NoNine = set2.replace("9", "6")
          (set1NoNine.contains(square.head) && set2NoNine.contains(square.last)) ||
            (set2NoNine.contains(square.head) && set1NoNine.contains(square.last))
        })
      })
    }).sum
      .toString
  }
}
