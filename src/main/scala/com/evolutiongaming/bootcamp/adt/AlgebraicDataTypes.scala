package com.evolutiongaming.bootcamp.adt

import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.Rank.rankPowers

import scala.annotation.tailrec

object AlgebraicDataTypes {

  sealed abstract case class Suit private (symbol: Char)
  object Suit {
    def of(symbol: Char): Option[Suit] =
      if (Set('s','d','c','h')(symbol)) Some(new Suit(symbol) {})
      else None
  }

  sealed abstract case class Rank private (symbol: Char) {
    val power: Int = rankPowers(symbol)
  }
  object Rank {

    def of(symbol: Char): Option[Rank] = {
      val validRanks = Set('2','3','4','5','6','7','8','9','T','J','Q','K','A')
      if (validRanks(symbol)) Some(new Rank(symbol) {})
      else None
    }

    lazy val rankPowers = Map(
      '2' -> 1, '3' -> 2, '4' -> 3, '5' -> 4, '6' -> 5, '7' -> 6, '8' -> 7,
      '9' -> 8, 'T' -> 9, 'J' -> 10, 'Q' -> 11, 'K' -> 12, 'A' -> 13)
  }

  final case class Card(rank: Rank, suit: Suit)

  /** Hand for Texas-holdem */
  sealed abstract case class Hand private (cards: List[Card])
  object Hand {
    def of(cards: List[Card]): Option[Hand] =
      if (cards.length == 2) Some(new Hand(cards) {})
      else None
  }

  sealed abstract case class Board private (cards: Set[Card])
  object Board {
    def of(cards: Set[Card]): Option[Board] =
      if (cards.size == 5) Some(new Board(cards) {})
      else None
  }

  sealed trait Combination {
    val power: Int
    val cards: Set[Card]
  }
  final case class HighCard(cards: Set[Card]) extends Combination {
    val power = 1
    def value: Card =
      cards.maxBy { _.rank.power }
  }
  final case class Pair(cards: Set[Card]) extends Combination {
    val power = 2
    def value: Option[Set[Card]] = {
      val cardsByRank = cards.toList.groupBy(_.rank)
      val numPairsMap = cardsByRank.count { case (_, cs) => cs.size == 2 }

      numPairsMap match {
        case 1 => cardsByRank.collectFirst { case (_, cs) if cs.size == 2 => cs.toSet }
        case _ => None
      }
    }
  }
  final case class TwoPairs(cards: Set[Card]) extends Combination {
    val power = 3
    def value: Option[Set[Set[Card]]] = {
      val cardsByRank = cards.groupBy(_.rank)
      val numPairsMap = cardsByRank.count { case (_, cs) => cs.size == 2 }

      numPairsMap match {
        case 2 => Some(cardsByRank.collect { case (_, cs) if cs.size == 2 => cs }.toSet)
        case _ => None
      }
    }
  }
  final case class ThreeOfKind(cards: Set[Card]) extends Combination {
    val power = 4
    def value: Option[Set[Card]] =
      cards.groupBy(_.rank).collectFirst { case (_, cs) if cs.size == 3 => cs }
  }
  final case class Straight(cards: Set[Card]) extends Combination {
    val power = 5
    def value: Option[Set[Card]] = {
      val weakestPowerStraight = List(1, 2, 3, 4, 13)
      val sortedPowerRanks = cards.toList.map(_.rank.power).sorted
      val isStraight = sortedPowerRanks.sliding(2)
        .forall { case List(r1, r2) => (r2 - r1) == 1 }

      Option.when(isStraight || sortedPowerRanks == weakestPowerStraight)(cards)
    }
  }
  final case class Flush(cards: Set[Card]) extends Combination {
    val power = 6
    def value: Option[Set[Card]] =
      Option.when(cards.map(_.suit).size == 1)(cards)
  }
  final case class FullHouse(cards: Set[Card]) extends Combination {
    val power = 7
    def value: Option[Set[Set[Card]]] = for {
        threeOfKind <- ThreeOfKind(cards).value
        pair        <- Pair(cards diff threeOfKind).value
      } yield Set(threeOfKind, pair)
  }
  final case class FourOfKind(cards: Set[Card]) extends Combination {
    val power = 8
    def value: Option[Set[Card]] =
      cards.groupBy(_.rank).collectFirst { case (_, cs) if cs.size == 4 => cs }
  }
  final case class StraightFlush(cards: Set[Card]) extends Combination {
    val power = 9
    def value: Option[Set[Card]] = for {
      _    <- Straight(cards).value
      _    <- Flush(cards).value
    } yield cards
  }
  final case class RoyalFlush(cards: Set[Card]) extends Combination {
    val power = 10
    def value: Option[Set[Card]] = for {
      straight <- Straight(cards).value
      if straight.map(_.rank.power).min == 9
      _        <- Flush(cards).value
    } yield cards
  }

  def bestCombination(hand: Hand, board: Board): Combination = {
    // Many if-s ... it does not look good
    def iterOverCombinations(cards: Set[Card]): Combination =
      if (RoyalFlush(cards).value.nonEmpty) RoyalFlush(hand.cards.toSet union board.cards)
      else if (StraightFlush(cards).value.nonEmpty) StraightFlush(hand.cards.toSet union board.cards)
      else if (FourOfKind(cards).value.nonEmpty) FourOfKind(hand.cards.toSet union board.cards)
      else if (FullHouse(cards).value.nonEmpty) FullHouse(hand.cards.toSet union board.cards)
      else if (Flush(cards).value.nonEmpty) Flush(hand.cards.toSet union board.cards)
      else if (Straight(cards).value.nonEmpty) Straight(hand.cards.toSet union board.cards)
      else if (ThreeOfKind(cards).value.nonEmpty) ThreeOfKind(hand.cards.toSet union board.cards)
      else if (TwoPairs(cards).value.nonEmpty) TwoPairs(hand.cards.toSet union board.cards)
      else if (Pair(cards).value.nonEmpty) Pair(hand.cards.toSet union board.cards)
      else HighCard(hand.cards.toSet union board.cards)

    (for {
      subsetFromBoard <- board.cards.subsets(5)
      subsetFromHand  <- hand.cards.toSet.subsets(2)
      subset <- (subsetFromBoard union subsetFromHand).subsets(5)
    } yield iterOverCombinations(subset)).maxBy(_.power)
  }

  def sortHands(hands: List[Hand], board: Board): List[Hand] = {
    val bestCombinations: Seq[(Hand, Combination)] = for (hand <- hands) yield hand -> bestCombination(hand, board)
    //println(bestCombinations.map(_._2).mkString("\n"))
    @tailrec
    def compare(comb1: Combination, comb2: Combination): Boolean = (comb1, comb2) match {
      case (c1, _) if c1.cards.isEmpty => true
      case (c1, c2) if c1.power != c2.power => c1.power < c2.power

      case (hc1: HighCard, hc2: HighCard) =>
        val (hc1Val, hc2Val) = (hc1.value, hc2.value)
        if (hc1Val.rank.power == hc2Val.rank.power) compare(HighCard(hc1.cards - hc1Val), HighCard(hc2.cards - hc2Val))
        else hc1Val.rank.power < hc2Val.rank.power

      case (p1: Pair, p2: Pair) =>
        val (p1Val, p2Val) = (p1.value.get, p2.value.get)
        if (p1Val.head.rank.power == p2Val.head.rank.power) compare(HighCard(p1.cards diff p1Val), HighCard(p2.cards diff p2Val))
        else p1Val.head.rank.power < p2Val.head.rank.power

      case (p1: TwoPairs, p2: TwoPairs) =>
        val (p1Val, p2Val) = (p1.value.get, p2.value.get)
        if (p1Val.head.maxBy(_.rank.power).rank.power == p2Val.head.maxBy(_.rank.power).rank.power)
          compare(Pair(p1.cards diff p1Val.maxBy(_.head.rank.power)), Pair(p2.cards diff p2Val.maxBy(_.head.rank.power)))
        else p1Val.head.maxBy(_.rank.power).rank.power < p2Val.head.maxBy(_.rank.power).rank.power

      case (t1: ThreeOfKind, t2: ThreeOfKind) =>
        val (t1Val, t2Val) = (t1.value.get, t2.value.get)
        if (t1Val.head.rank.power == t2Val.head.rank.power)
          compare(HighCard(t1.cards diff t1Val), HighCard(t2.cards diff t2Val))
        else t1Val.head.rank.power < t2Val.head.rank.power

      case (s1: Straight, s2: Straight) =>
        if (s1.cards.map(_.rank) == s2.cards.map(_.rank)) true
        else if (s1.cards.map(_.rank.power).max == Rank.rankPowers('A') && s1.cards.map(_.rank.power).min == Rank.rankPowers('2')) true
        else s1.cards.map(_.rank.power).max < s2.cards.map(_.rank.power).max

      case (f1: Flush, f2: Flush) =>
        val (h1Val, h2Val) = (HighCard(f1.cards).value, HighCard(f2.cards).value)
        if (h1Val.rank.power == h2Val.rank.power) compare(HighCard(f1.cards - h1Val), HighCard(f2.cards - h2Val))
        else h1Val.rank.power < h2Val.rank.power

      case (fh1: FullHouse, fh2: FullHouse) =>
        val (t1Val, t2Val) = (fh1.value.get.filter(_.size == 3).head, fh2.value.get.filter(_.size == 3).head)
        val (p1Val, p2Val) = (fh1.value.get.filter(_.size == 2).head, fh2.value.get.filter(_.size == 2).head)
        if (t1Val.head.rank.power == t2Val.head.rank.power) {
          if (p1Val.head.rank.power == p2Val.head.rank.power) true
          else p1Val.head.rank.power < p2Val.head.rank.power
        }
        else t1Val.head.rank.power < t2Val.head.rank.power

      case (fk1: FourOfKind, fk2: FourOfKind) =>
        val (fk1Val, fk2Val) = (fk1.value.get, fk2.value.get)
        if (fk1Val.head.rank.power == fk2Val.head.rank.power)
          compare(HighCard(fk1.cards diff fk1Val), HighCard(fk2.cards diff fk2Val))
        else fk1Val.head.rank.power < fk2Val.head.rank.power

      case (sf1: StraightFlush, sf2: StraightFlush) =>
        if (sf1.cards.map(_.rank) == sf2.cards.map(_.rank)) true
        else if (sf1.cards.map(_.rank.power).max == Rank.rankPowers('A') && sf1.cards.map(_.rank.power).min == Rank.rankPowers('2')) true
        else sf1.cards.map(_.rank.power).max < sf2.cards.map(_.rank.power).max

      case (_: RoyalFlush, _: RoyalFlush) => true
    }
    bestCombinations.sortWith { case ((_, comb1), (_, comb2)) => compare(comb1, comb2) }.map(_._1).toList
  }

  def main(args: Array[String]): Unit = {

    val board = Board.of(Set(
      Card(Rank.of('5').get, Suit.of('c').get),
      Card(Rank.of('6').get, Suit.of('d').get),
      Card(Rank.of('A').get, Suit.of('c').get),
      Card(Rank.of('A').get, Suit.of('s').get),
      Card(Rank.of('Q').get, Suit.of('s').get))).get

    val hands = List(
      Hand.of(List(Card(Rank.of('K').get, Suit.of('s').get),
        Card(Rank.of('4').get, Suit.of('c').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('d').get),
        Card(Rank.of('J').get, Suit.of('s').get))).get,
      Hand.of(List(Card(Rank.of('2').get, Suit.of('h').get),
        Card(Rank.of('A').get, Suit.of('h').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('h').get),
        Card(Rank.of('4').get, Suit.of('h').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('c').get),
        Card(Rank.of('7').get, Suit.of('h').get))).get,
      Hand.of(List(Card(Rank.of('6').get, Suit.of('h').get),
        Card(Rank.of('7').get, Suit.of('d').get))).get,
      Hand.of(List(Card(Rank.of('2').get, Suit.of('c').get),
        Card(Rank.of('J').get, Suit.of('c').get))).get)

    val expectedOutput = List(
      Hand.of(List(Card(Rank.of('2').get, Suit.of('c').get),
        Card(Rank.of('J').get, Suit.of('c').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('h').get),
        Card(Rank.of('4').get, Suit.of('h').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('s').get),
        Card(Rank.of('4').get, Suit.of('c').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('c').get),
        Card(Rank.of('7').get, Suit.of('h').get))).get,
      Hand.of(List(Card(Rank.of('K').get, Suit.of('d').get),
        Card(Rank.of('J').get, Suit.of('s').get))).get,
      Hand.of(List(Card(Rank.of('6').get, Suit.of('h').get),
        Card(Rank.of('7').get, Suit.of('d').get))).get,
      Hand.of(List(Card(Rank.of('2').get, Suit.of('h').get),
        Card(Rank.of('A').get, Suit.of('h').get))).get)

    // texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc
    // 2cJc Kh4h Ks4c Kc7h KdJs 6h7d 2hAh
    if (sortHands(hands, board) == expectedOutput) println("output match with the expected output")
    else println("output doesn't match with the expected output")
  }
}
