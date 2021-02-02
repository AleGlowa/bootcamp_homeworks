package com.evolutiongaming.bootcamp.basics

import scala.annotation.tailrec

object Basics {

  def lcm(a: Int, b: Int): Option[Int] = (a > 0, b > 0) match {
    case (true, true) => Some(a * b / gcd(a, b))
    case _ => None
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    // convert negative numbers to positive ones
    val (aNonNeg, bNonNeg) = (Math.abs(a), Math.abs(b))
    val (smaller, larger) = (aNonNeg min bNonNeg, aNonNeg max bNonNeg)
    if (smaller == 0) larger else gcd(smaller, larger % smaller)
  }
}
