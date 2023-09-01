package net.projecteuler.kurtsu

import scala.math.{pow, floor, ceil}

object Problem62 extends Solution {
  override val problemNum: Int = 62
  override val difficulty: Int = 15

  override final def solution(): String =
    val numOfPerm = 5
    val cubeRoot = pow(_, 1.0 / 3.0)
    val lCube = (n: Long) => n * n * n
    val isPerm = (x: String, y: Long) => x == y.toString.sorted

    val x = LazyList.from(7)
      .map(n => pow(10.0, n))
      .map(low => ceil(cubeRoot(low)).toLong to floor(cubeRoot(10 * low - 1)).toLong)
      .map(roots => roots.map(lCube))
      .find(cubes =>
        // maybe we sort them each first...
        cubes.exists(cube =>
          val isPermCube = isPerm(cube.toString.sorted, _)
          if cubes.count(isPermCube(_)) == numOfPerm then
            println(cube)
            true
          else false
        )
      )
    "???"

  def main(args: Array[String]): Unit = {
    val t = System.currentTimeMillis
    solution()
    println(s"runtime: ${System.currentTimeMillis - t} ms")
  }
}
