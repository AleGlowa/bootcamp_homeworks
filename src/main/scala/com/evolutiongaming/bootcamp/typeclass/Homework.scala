package com.evolutiongaming.bootcamp.typeclass

import scala.util.matching.Regex

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

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)


  implicit val userParser: Parse[User] = str => {
    val userReg: Regex = raw"User\{id=(\d+),\s*name=([a-zA-Z]+)\}".r
    userReg.findFirstMatchIn(str) match {
      case None => Left("The given user can't be parsed")
      case Some(data) => Right(User(data.group(1), data.group(2)))
    }
  }

  implicit class ParseSyntax(x: String) {
    def parse[T](implicit parser: Parse[T]): Either[Error, T] = parser.parse(x)
  }

  // Unsuccessful case
  println("lalala".parse[User])
  // Successful case
  println("User{id=375137625,     name=oLeG}".parse[User])
}

object Task4 extends App {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  trait Equals[T] {
    def === (x: T, y: T): Boolean
  }

  implicit class EqualsSyntax[T](x: T) {
    def === (y: T): Boolean = x == y
  }
  import Task3.User
  println(User("007", "JamesBond") === User("700", "BondJames"))
  println(98 === 'b')
  // println(1 === 1.0) -- won't compile
  // remember `a method b` is `a.method(b)`
}

object AdvancedHomework extends App {
  trait FlatMap[F[_]] {
    def flatMap[A, B](x: F[A])(f: A => IterableOnce[B]): F[B]
  }
  object FlatMap {
    def apply[F[_] : FlatMap]: FlatMap[F] = implicitly[FlatMap[F]]
  }
  implicit class FlatMapOps[F[_] : FlatMap, A](x: F[A]) {
    def flatMap[B](f: A => IterableOnce[B]): F[B] = FlatMap[F].flatMap(x)(f)
  }

  implicit val myListFunctor: FlatMap[MyList] = new FlatMap[MyList] {
    def flatMap[A, B](x: MyList[A])(f: A => IterableOnce[B]): MyList[B] =
      List(x.x: _*).flatMap(f).toMyList
  }

  // Artificial extension class for original `List`
  implicit class ExtList[A](x: List[A]) {
    def toMyList: MyList[A] = MyList(x: _*)
  }
  final case class MyList[+A](x: A*)

  val res = MyList(1, 2, 3, 4, 5).flatMap(x => Set(x * math.Pi))

  println(res)
}
