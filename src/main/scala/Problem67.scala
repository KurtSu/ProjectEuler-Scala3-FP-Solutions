package net.projecteuler.kurtsu

import scala.annotation.tailrec

object Problem67 extends Solution {
  override val problemNum: Int = 67
  override val difficulty: Int = 5

  override final def solution(): String =
    val fileURL = "https://projecteuler.net/resources/documents/0067_triangle.txt"
    val buffer = scala.io.Source.fromURL(fileURL)
    val triangle = try parseTriangle(buffer.getLines) finally buffer.close
    maxPathSum(triangle).toString

  final def parseTriangle(lines: Iterator[String]): List[Array[Int]] =
    lines.map(_.split(' ').map(_.toInt)).toList.reverse

  @tailrec
  final def maxPathSum(triangle: List[Array[Int]]): Int =
    triangle match
      case Nil => 0 // should not happen

      case bottom :: Nil => bottom.head

      case bottom :: top =>
        val newBottom = bottom.dropRight(1)
          .zip(bottom.tail)
          .map { case(l, r) => math.max(l, r) }
          .zip(top.head)
          .map { case(b, t) => b + t }
        maxPathSum(newBottom :: top.tail)
}
