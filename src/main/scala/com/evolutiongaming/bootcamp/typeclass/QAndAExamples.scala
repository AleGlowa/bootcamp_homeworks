package com.evolutiongaming.bootcamp.typeclass

object QAndAExamples {

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  // 4. Semigroupal
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }
  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly
  }
  implicit class SemigroupalOps[F[_]: Semigroupal, A](x: F[A]) {
    def product[B](y: F[B]): F[(A, B)] = Semigroupal[F].product(x, y)
  }

  // `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class extTuple[F[_]: Functor : Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap(f.tupled)
    }
  }

  // 4.4. Implement Semigroupal for Map
  implicit def semigroupalMap[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
    def product[V1, V2](fa: Map[K, V1], fb: Map[K, V2]): Map[K, (V1, V2)] =
      (fa.keySet intersect fb.keySet).map { k => k -> (fa(k), fb(k)) }.toMap
  }
  implicit def functorMap[K]: Functor[Map[K, *]] = new Functor[Map[K, *]] {
    def fmap[V1, V2](fa: Map[K, V1])(f: V1 => V2): Map[K, V2] =
      fa.view.mapValues(f).toMap
  }
  // semigroupalMap[Int].product(Map(1 -> "a", 2 -> "b", 3 -> "x"), Map(2 -> "c", 3 -> "d")) == Map(2 -> ("b", "c"), 3 -> ("x", "d"))
  // .mapN(_ + _) == Map(2 -> "bc")

  (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }
  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }
  implicit class ApplicativeOps[F[_] : Applicative, A](x: F[A]) {
    def pure: F[A] = Applicative[F].pure(x)
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val applicativeOption: Applicative[Option] = new Applicative[Option] {
    def pure[A](x: A): Option[A] = Option(x)
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      for (a <- fa; b <- fb) yield (a, b)

    def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def applicativeEither[L]: Applicative[Either[L, *]] = new Applicative[Either[L, *]] {
    override def pure[A](x: A): Either[L, A] = Right(x)
    override def product[A, B](fa: Either[L, A], fb: Either[L, B]): Either[L, (A, B)] =
      for (a <- fa; b <- fb) yield (a, b)

    override def fmap[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.map(f)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  //def traverse[F[_] : Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
  //  as.map()
  //}

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    ???
  }

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeft[T, S](ft: F[T], s: S)(f: (S, T) => S): S
  }
  object Foldable {
    def apply[F[_] : Foldable]: Foldable[F] = implicitly[Foldable[F]]
  }
  implicit class FoldableOps[F[_]: Foldable, T](ft: F[T]) {
    def foldLeft[S](s: S)(f: (S, T) => S): S = Foldable[F].foldLeft(ft, s)(f)
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    def foldLeft[T, S](ft: List[T], s: S)(f: (S, T) => S): S =
      ft.foldLeft(s)(f)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  def traverse[F[_] : Applicative, G[_] : Foldable, A, B](as: G[A])(f: A => F[B]): F[G[B]] = {
    ???
  }
  //def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???
}

object CodecsExercise {
  sealed trait Json
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  // Encoder
  trait Encoder[A] {
    def toJson(a: A): Json
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // Decoder
  trait Decoder[A] {
    def fromJson(json: Json): Option[A]
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }

  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }
  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }
  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  List(1, 2, 3).fmap(_ + 1)
  // Exercise 7. Implement Functor for decoder: `implicit val decoderFunctor`
  implicit val decoderFunctor: Functor[Decoder] = new Functor[Decoder] {
    def fmap[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = json => fa.fromJson(json).map(f)
  }

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: `trait Contravariant`
  // 2. Typeclass Summoner: `object Contravariant`
  // 3. Typeclass Ops: `implicit class ContravariantOps`
  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }
  object Contravariant {
    def apply[F[_]: Contravariant]: Contravariant[F] = implicitly[Contravariant[F]]
  }
  implicit class ContravariantOps[F[_]: Contravariant, A](fa: F[A]) {
    def contramap[B](f: B => A): F[B] = Contravariant[F].contramap(fa)(f)
  }

  // Exercise 9. Implement Contravariant for encoder: `implicit val encoderContravariant`
  implicit val encoderContravariant: Contravariant[Encoder] = new Contravariant[Encoder] {
    def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = a => fa.toJson(f(a))
  }

  // Functions Example
  val foo1: String => Int = _.length
  val foo2: Boolean => String = if (_) "100" else "1"

  // Exercise 10. Implement Functor and Contravariant for functions:
  implicit def functionFunctor[A]: Functor[A => *] = new Functor[A => *] {
    def fmap[B, C](fa: A => B)(f: B => C): A => C = f.compose(fa)
  }
  implicit def functionContravariant[C]: Contravariant[* => C] = new Contravariant[* => C] {
    def contramap[A, B](fa: A => C)(f: B => A): B => C = f.andThen(fa)
  }

  val foo3: Boolean => Int = functionFunctor.fmap(foo2)(foo1)
  val foo4: Boolean => Int = functionContravariant.contramap(foo1)(foo2)
}

object FoldableExercise {
  object FoldableExercise {
    trait Foldable[F[_]] {
      def foldLeft[T, S](ft: F[T], s: S)(f: (S, T) => S): S
    }

    implicit class FoldableOps[F[_]: Foldable, T](ft: F[T]) {
      def foldLeft[S](s: S)(f: (S, T) => S): S = implicitly[Foldable[F]].foldLeft(ft, s)(f)
    }

    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      override def foldLeft[T, S](ft: Option[T], s: S)(f: (S, T) => S): S =
        ft match {
          case None    => s
          case Some(t) => f(s, t)
        }
    }
    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      override def foldLeft[T, S](ft: List[T], s: S)(f: (S, T) => S): S =
        ft.foldLeft(s)(f)
    }

    final case class Triple[T](
                                v1: T,
                                v2: T,
                                v3: T,
                              )

    /*
    Part 1.

    Define an Foldable instance for Triple (should behave like a collection of 3 elements)
     */

    implicit val tripleFoldable: Foldable[Triple] = new Foldable[Triple] {
      def foldLeft[T, S](ft: Triple[T], s: S)(f: (S, T) => S): S =
        f(f(f(s, ft.v1), ft.v2), ft.v3)
    }

    /*
    Part 2.

    Define another type-class - Summable[T] which should give us methods:
    - def plus(left: T, right: T): T
    - def zero: T

    Define Summable[T] instances for:
    - any T which has the standard library Numeric[T] type-class provided
    - Set[S] - zero should be Set.empty and plus should merge sets with + operation
     */
    trait Summable[T] {
      def plus(left: T, right: T): T
      def zero: T
    }
    object Summable {
      def apply[T : Summable]: Summable[T] = implicitly[Summable[T]]
    }

    implicit def numericSummable[T : Numeric]: Summable[T] = new Summable[T] {
      def plus(left: T, right: T): T = Numeric[T].plus(left, right)
      def zero: T = Numeric[T].zero
    }
    implicit def setSummable[S]: Summable[Set[S]] = new Summable[Set[S]] {
      def plus(left: Set[S], right: Set[S]): Set[S] = left union right
      def zero: Set[S] = Set.empty
    }

    /*
    Part 3.

    And finally - define generic collection sum method which works on any F[T]
    where F is Foldable (F[_]: Foldable) and T is Summable (T: Summable)!

    def genericSum... - work out the right method signature, should take F[T] and return T
     */
    def genericSum[T : Summable, F[_] : Foldable](fa: F[T]): T =
      fa.foldLeft(Summable[T].zero) { Summable[T].plus }
  }
}