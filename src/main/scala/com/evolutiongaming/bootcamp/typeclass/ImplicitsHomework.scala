package com.evolutiongaming.bootcamp.typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner) //implement the syntax!
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        lazy val (keyScore, valueScore) = (implicitly[GetSizeScore[K]], implicitly[GetSizeScore[V]])
        def getScore(element: (K, V)): SizeScore = keyScore(element._1) + valueScore(element._2)

        val latestScore = getScore((key, value))
        val mapScore = map.foldLeft(0) { (s, x) => s + getScore(x) }
        val score = mapScore + latestScore

        @tailrec
        def traverse(score: SizeScore): Unit = {
          if (score > maxSizeScore) {
            val toDelete = map.head
            map -= toDelete._1
            traverse(score - getScore(toDelete))
          }
          else map += ((key, value))
        }
        traverse(score)
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[K, V](f: Map[K, V]): Iterator[K] = f.keysIterator
        override def iterator2[K, V](f: Map[K, V]): Iterator[V] = f.valuesIterator
      }
      implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[K, V](f: PackedMultiMap[K, V]): Iterator[K] = f.inner.view.map(_._1).iterator
        override def iterator2[K, V](f: PackedMultiMap[K, V]): Iterator[V] = f.inner.view.map(_._2).iterator
      }
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */
      val HeaderSize: Int = 12
      implicit val sizeScoreByte: GetSizeScore[Byte] = _ => 1
      implicit val sizeScoreChar: GetSizeScore[Char] = _ => 2
      implicit val sizeScoreInt: GetSizeScore[Int] = _ => 4
      implicit val sizeScoreLong: GetSizeScore[Long] = _ => 8
      implicit val sizeScoreStr: GetSizeScore[String] = str => HeaderSize + str.length * 2

      implicit def sizeScoreIterable[T, F[_] <: Iterable[_]](implicit elScore: GetSizeScore[T], iterate: Iterate[F]): GetSizeScore[F[T]] =
        seq => iterate.iterator(seq).foldLeft(HeaderSize) { _ + elScore(_) }
      implicit def sizeScoreArr[T](implicit elScore: GetSizeScore[T]): GetSizeScore[Array[T]] = arr =>
        arrayIterate.iterator(arr).foldLeft(HeaderSize) { _ + elScore(_) }

      implicit def sizeScoreMap[K, V, F[_, _]](implicit keyScore: GetSizeScore[K], valueScore: GetSizeScore[V], iterate: Iterate2[F]): GetSizeScore[F[K, V]] =
        map => iterate.iterator1(map).foldLeft(0) { _ + keyScore(_) } +
          iterate.iterator2(map).foldLeft(0) { _ + valueScore(_) } + HeaderSize
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import instances._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    implicit val sizeScoreFbiNote: GetSizeScore[FbiNote] = fbiNote =>
      implicitly[GetSizeScore[String]].apply(fbiNote.month) +
        implicitly[GetSizeScore[Char]].apply(fbiNote.favouriteChar) +
        implicitly[GetSizeScore[Long]].apply(fbiNote.watchedPewDiePieTimes) + HeaderSize

    implicit val sizeScoreTwit: GetSizeScore[Twit] = twit =>
      implicitly[GetSizeScore[Long]].apply(twit.id) +
        implicitly[GetSizeScore[Int]].apply(twit.userId) +
        implicitly[GetSizeScore[Iterable[String]]].apply(twit.hashTags) +
        implicitly[GetSizeScore[PackedMultiMap[String, String]]].apply(twit.attributes) +
        implicitly[GetSizeScore[Iterable[FbiNote]]].apply(twit.fbiNotes) + HeaderSize

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      def put(twit: Twit): Unit = cache.put(twit.id, twit)
      def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
