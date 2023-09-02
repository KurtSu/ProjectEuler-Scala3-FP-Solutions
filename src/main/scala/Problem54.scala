package net.projecteuler.kurtsu

object Problem54 extends Solution {
  override val problemNum: Int = 54
  override val difficulty: Int = 10

  override final def solution(): String =
    val fileURL = "https://projecteuler.net/resources/documents/0054_poker.txt"
    val buffer = scala.io.Source.fromURL(fileURL)
    try buffer.getLines.count(parseLine).toString finally buffer.close

  private case class Card(value: Char, suit: Char) {
    val numericalValue: Int =
      if value.isDigit then value - '0'
      else Map('T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14)(value)
  }

  private case class Hand(cards: Array[Card]) {
    private val cardsNumerical = cards.map(_.numericalValue).sorted
    private val cardsSuit = cards.map(_.suit)
    private val cardsNumericalCountMap =
      cardsNumerical.groupBy(identity).map { case (k, v) => (k, v.length)}

    private def highestOfCount(numericalCountMap: Map[Int, Int])(count: Int): Option[Int] =
      numericalCountMap
        .filter((_, v) => v == count)
        .map { case (k, _) => k }
        .maxOption

    private def highCard: Option[Int] = Some(cardsNumerical.max)

    private def onePair: Option[Int] = highestOfCount(cardsNumericalCountMap)(2)

    private def twoPair: Option[Int] =
      onePair match
        case None => None
        case Some(major) =>
          highestOfCount(cardsNumericalCountMap.removed(major))(2) match
            case Some(_) => Some(major)
            case None => None

    private def threeOfAKind: Option[Int] = highestOfCount(cardsNumericalCountMap)(3)

    private def straight: Option[Int] =
      if cardsNumerical
        .sorted
        .sliding(2)
        .forall { case Array(a, b) => a + 1 == b case _ => false } then
        Some(cardsNumerical.sorted.min)
      else None

    private def flush: Option[Int] =
      val allSuits = cardsSuit.distinct
      if 1 == allSuits.length then Some(0)
      else None

    private def fullHouse: Option[Int] =
      threeOfAKind match
        case None => None
        case Some(major) =>
          highestOfCount(cardsNumericalCountMap.removed(major))(2) match
            case Some(_) => Some(major)
            case None => None

    private def fourOfAKind: Option[Int] = highestOfCount(cardsNumericalCountMap)(4)

    private def straightFlush: Option[Int] =
      (straight, flush) match
        case (Some(card), Some(_)) => Some(card)
        case _ => None

    private def royalFlush: Option[Int] =
      straightFlush match
        case Some(10) => Some(0)
        case _ => None

    private def test: List[Int] =
      List(royalFlush, straightFlush, fourOfAKind, fullHouse, flush,
        straight, threeOfAKind, twoPair, onePair, highCard)
        .map {
          case Some(n) => n
          case None => -1
        }

    def wins(that: Hand): Boolean =
      val sideBySide = this.test.zip(that.test)
      val idxOfDiff = sideBySide.indexWhere((a, b) => a != b)
      sideBySide(idxOfDiff)._1 > sideBySide(idxOfDiff)._2
  }

  private def parseCard(s: String) = Card(s(0), s(1))

  private def parseHand(s: Array[String]) = Hand(s.map(parseCard))

  private def parseLine(line: String): Boolean =
    val (s1, s2) = line.split(' ').splitAt(5)
    val (p1, p2) = (parseHand(s1), parseHand(s2))
    p1 wins p2
}
