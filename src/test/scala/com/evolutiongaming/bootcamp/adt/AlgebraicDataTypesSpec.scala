package com.evolutiongaming.bootcamp.adt

import org.scalatest.flatspec.AnyFlatSpec
import AlgebraicDataTypes._

object AlgebraicDataTypesSpec extends AnyFlatSpec {

  "Test case" should "be correct" in {
    val board = Board.of(Set(
      Card(Rank.of('5').get, Suit.of('c').get),
      Card(Rank.of('6').get, Suit.of('d').get),
      Card(Rank.of('A').get, Suit.of('c').get),
      Card(Rank.of('A').get, Suit.of('s').get),
      Card(Rank.of('Q').get, Suit.of('s').get)))

    val hands = Set(
      Hand.of(List(Card(Rank.of('K').get, Suit.of('s').get),
        Card(Rank.of('4').get, Suit.of('c').get))),
      Hand.of(List(Card(Rank.of('K').get, Suit.of('d').get),
        Card(Rank.of('J').get, Suit.of('s').get))),
      Hand.of(List(Card(Rank.of('2').get, Suit.of('h').get),
        Card(Rank.of('A').get, Suit.of('h').get))),
      Hand.of(List(Card(Rank.of('K').get, Suit.of('h').get),
        Card(Rank.of('4').get, Suit.of('h').get))),
      Hand.of(List(Card(Rank.of('K').get, Suit.of('c').get),
        Card(Rank.of('7').get, Suit.of('h').get))),
      Hand.of(List(Card(Rank.of('6').get, Suit.of('h').get),
        Card(Rank.of('7').get, Suit.of('d').get))),
      Hand.of(List(Card(Rank.of('2').get, Suit.of('c').get),
        Card(Rank.of('J').get, Suit.of('c').get))))



  }
}
