// MY LEETCODE PROFILE IS HERE: https://leetcode.com/AleGlowa/

package com.evolutiongaming.bootcamp.basics

import scala.annotation.tailrec

object Collections {

  // try to implement min different ways (fold, reduce, recursion)
  // tailrec implementation
  /*
  @tailrec
  def min(list: List[Int]): Option[Int] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => if (xs.head < x) min(xs.tail) else min(x :: xs.tail)
  }
  */

  // foldLeft implementation
  def min(list: List[Int]): Option[Int] = list match {
    case Nil => None
    case x :: _ => Some(list.foldLeft(x) { _ min _ })
  }

  // reduce implementation
  /*
  def min(list: List[Int]): Option[Int] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: _ => Some(list.reduce(_ min _))
  }
  */

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    @tailrec
    def iter(xs: List[T], res: List[T]): List[T] = xs match {
      case Nil => res
      case _ :: _ => iter(xs.init, xs.foldLeft(zero)(f) :: res)
    }
    zero :: iter(list, Nil)
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    @tailrec
    def iter(s: String, res: List[(Char, Int)]): List[(Char, Int)] =
      if (s.isEmpty) res
      else {
        val ch = s.head
        val sameChLen = s.takeWhile(_ == ch).length
        iter(s.drop(sameChLen), (ch, sameChLen) :: res)
      }

    iter(s.reverse, Nil)
  }
}

object LeetCode {

  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.tail.scanLeft(nums.head)(_ + _)
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val (firstHalf, secondHalf) = nums splitAt n
    (firstHalf zip secondHalf).flatMap { case (x, y) => Array(x, y) }
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val greatest = candies.max
    candies.map(_ + extraCandies >= greatest)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    points.map(_(0)).sorted.sliding(2).map { case Array(x, y) => y - x }.max
  }

  def maxDepth(s: String): Int =
    s.foldLeft(List(0)) { (acc, ch) =>
      if (ch == '(') (acc.head + 1) :: acc
      else if (ch == ')') (acc.head - 1) :: acc
      else acc
    }.max

  def balancedStringSplit(s: String): Int =
    s.foldLeft(List(0)) { (acc, ch) =>
      if (ch == 'R') (acc.head + 1) :: acc
      else if (ch == 'L') (acc.head - 1) :: acc
      else acc
    }.count(_ == 0) - 1

  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val firstRow = mat.head
    val (m, n) = (mat.length, firstRow.length)
    val answer = Array.ofDim[Int](m, n)
    for {
      i <- mat.indices
      j <- firstRow.indices
      r = ((i - K) to (i + K)).dropWhile(_ < 0).takeWhile(_ < m)
      c = ((j - K) to (j + K)).dropWhile(_ < 0).takeWhile(_ < n)
      blockRows = mat.slice(r.start, r.end + 1)
      blockSum = blockRows.foldLeft(0) { (sum, row) => sum + row.slice(c.start, c.end + 1).sum }
    } yield answer(i)(j) = blockSum
    answer
  }
}
