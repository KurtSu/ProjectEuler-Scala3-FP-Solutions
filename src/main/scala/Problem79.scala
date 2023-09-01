package net.projecteuler.kurtsu

import scala.annotation.tailrec

object Problem79 extends Solution {
  override val problemNum: Int = 79
  override val difficulty: Int = 5

  override final def solution(): String =
    val fileURL = "https://projecteuler.net/resources/documents/0079_keylog.txt"
    val buffer = scala.io.Source.fromURL(fileURL)
    val keys = try buffer.getLines.toList finally buffer.close
    minPasscode(keys, "", Nil)
    ""

  private case class Order(less: Char, more: Char)

  @tailrec
  private def minPasscode(keys: List[String], alphabet: String, orders: List[Order]): String =
    keys match
      case Nil =>
        println(alphabet)
        println(orders)
        "TODO"
      case key :: tail =>
        val newOrders = key.dropRight(1)
          .zip(key.tail)
          .map { case (less, more) => Order(less, more) }
          .toList
        minPasscode(tail, (alphabet+key).distinct, (orders ++ newOrders).distinct)
  def main(args: Array[String]): Unit = {
    println(solution())
  }
}
