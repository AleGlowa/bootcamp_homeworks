package com.evolutiongaming.bootcamp.adt

import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.Rank.rankPowers

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

  final case class HighCard(cards: Set[Card]) {
    val power = 1
    def value: Card =
      cards.maxBy { _.rank.power }
  }
  final case class Pair(cards: Set[Card]) {
    val power = 2
    def value: Option[Set[Card]] = {
      val cardsByRank = cards.toList.groupBy(_.rank)
      val numPairsMap = cardsByRank.count { case (_, cs) => cs.size == 2 }

      numPairsMap match {
        case 0 => None
        case 1 => cardsByRank.collectFirst { case (_, cs) if cs.size == 2 => cs.toSet }
        case 2 =>
          val pairs = cardsByRank.collect { case (_, cs) if cs.size == 2 => cs }.toList
          val betterCardPair = HighCard(Set(pairs.head.head, pairs.tail.head)).value
          if (betterCardPair == pairs.head.head) Some(Set(betterCardPair, pairs.head.last))
          else Some(Set(betterCardPair, pairs.tail.last))
      }
    }
  }
  final case class TwoPairs(cards: Set[Card]) {
    val power = 3
    def value: Option[Set[Set[Card]]] = {
      val cardsByRank = cards.groupBy(_.rank)
      val numPairsMap = cardsByRank.count { case (_, cs) => cs.size == 2 }

      numPairsMap match {
        case 0 | 1 => None
        case 2 => Some(cardsByRank.collect { case (_, cs) if cs.size == 2 => cs }.toSet)
      }
    }
  }
  final case class ThreeOfKind(cards: Set[Card]) {
    val power = 4
    def value: Option[Set[Card]] =
      cards.groupBy(_.rank).collectFirst { case (_, cs) if cs.size == 3 => cs }
  }
  final case class Straight(cards: Set[Card]) {
    val power = 5
    def value: Option[Set[Card]] = {
      val weakestPowerStraight = List(1, 2, 3, 4, 13)
      val sortedPowerRanks = cards.toList.map(_.rank.power).sorted
      val isStraight = sortedPowerRanks.sliding(2)
        .forall { case List(r1, r2) => (r2 - r1) == 1 }

      Option.when(isStraight || sortedPowerRanks == weakestPowerStraight)(cards)
    }
  }
  final case class Flush(cards: Set[Card]) {
    val power = 6
    def value: Option[Set[Card]] =
      Option.when(cards.map(_.suit).size == 1)(cards)
  }
  final case class FullHouse(cards: Set[Card]) {
    val power = 7
    def value: Option[Set[Set[Card]]] = for {
        threeOfKind <- ThreeOfKind(cards).value
        pair        <- Pair(cards diff threeOfKind).value
      } yield Set(threeOfKind, pair)
  }
  final case class FourOfKind(cards: Set[Card]) {
    val power = 8
    def value: Option[Set[Card]] =
      cards.groupBy(_.rank).collectFirst { case (_, cs) if cs.size == 4 => cs }
  }
  final case class StraightFlush(cards: Set[Card]) {
    val power = 9
    def value: Option[Set[Card]] = for {
      straight <- Straight(cards).value
      flush    <- Flush(cards).value
    } yield Set(cards)
  }
  final case class RoyalFlush(cards: Set[Card]) {
    val power = 10
    def value: Option[Set[Card]] = for {
      straight <- Straight(cards).value
      if straight.map(_.rank.power).min == 9
      flush    <- Flush(cards).value
    } yield Set(cards)
  }

  def sortHands(hands: Set[Hand], board: Board): List[Hand] = {
    val bestCombinations = for (hand <- hands) yield bestCombination(hand, board)
    bestCombinations.sortBy
  }

  def main(args: Array[String]): Unit = {

  }
}
