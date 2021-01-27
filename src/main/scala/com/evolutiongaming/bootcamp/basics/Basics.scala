package com.evolutiongaming.bootcamp.basics

object Basics {

  // `lcm` returns -1 for invalid arguments (a <= 0 or b <= 0)
  def lcm(a: Int, b: Int): Int = if (a > 0 && b > 0) a * b / gcd(a, b) else -1

  def gcd(a: Int, b: Int): Int = {
    // case for two 0s
    if (a == 0 && b == 0) Int.MaxValue
    else {
      // convert negative numbers to positive ones
      val (aNonNeg, bNonNeg) = (Math.abs(a), Math.abs(b))
      val (smaller, larger) = (aNonNeg min bNonNeg, aNonNeg max bNonNeg)
      if (smaller == 0) larger else gcd(smaller, larger % smaller)
    }
  }
}
