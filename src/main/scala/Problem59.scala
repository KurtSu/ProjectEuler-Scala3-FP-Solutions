package net.projecteuler.kurtsu

object Problem59 extends Solution {
  override val problemNum: Int = 59
  override val difficulty: Int = 5

  override final def solution(): String =
    val fileURL = "https://projecteuler.net/resources/documents/0059_cipher.txt"
    val buffer = scala.io.Source.fromURL(fileURL)
    val encrypted = buffer.mkString.split(',').map(_.toInt)
    buffer.close

    val allLowerCase = ('a' to 'z').map(_.toInt)
    // Cartesian product of 3 sets.
    val allPasswords = for {
      a <- allLowerCase
      b <- allLowerCase
      c <- allLowerCase
    } yield Array(a, b, c)

    val decrypted = allPasswords
      .map(decrypt(encrypted, _))
      .filter(s =>
        List("the", "to", "and", "of", "a", "in", "that")
          .forall(s.contains(_))
      )
      .head

    decrypted
      .map(_.toInt)
      .sum
      .toString

  private def decrypt(encrypted: Array[Int], password: Array[Int]): String =
    encrypted.zipWithIndex
      .map { case (e, idx) =>
        e ^ password(idx % password.length)
      }
      .map(_.toChar)
      .mkString
}
