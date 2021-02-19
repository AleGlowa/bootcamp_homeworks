package com.evolutiongaming.bootcamp.adt

import org.scalatest.flatspec.AnyFlatSpec
import AlgebraicDataTypes._
import org.scalatest.matchers.should.Matchers._

object AlgebraicDataTypesSpec extends AnyFlatSpec {

  "Test case" should "be correct" in {
    // texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc
    // 2cJc Kh4h Ks4c Kc7h KdJs 6h7d 2hAh
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


    sortHands(hands, board) shouldEqual expectedOutput
  }
}
