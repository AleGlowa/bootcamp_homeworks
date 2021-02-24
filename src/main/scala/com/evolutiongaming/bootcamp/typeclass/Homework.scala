package com.evolutiongaming.bootcamp.typeclass

object TypeclassTask extends App {

  // Why am I not a Typeclass?
  trait HashCode[A] {
    def hash(data: A): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit hashFunc: HashCode[A]): Int =
      hashFunc.hash(x)
  }
  def printHash[A: HashCode](x: A): Unit =
    println(s"Hash for $x is: ${x.hash}")

  implicit val strToHash: HashCode[String] = str => str.map(_.toInt).sum / str.length
  "abc".hash
  printHash("abc")
}

object Task1 extends App {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
  val btcPrices = List(Money(100), Money(4000), Money(10000), Money(4000), Money(56000), Money(48000))
  println(btcPrices.sorted)
}

object Task2 extends App {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val userIntroducer: Show[User] = user => s"Welcome my favourite User{id=${user.id},name=${user.name}}!"

  implicit class ShowSyntax[T](x: T) {
    def show(implicit introducer: Show[T]): String = introducer.show(x)
  }

  println(User("1", "Oleg").show)
}


