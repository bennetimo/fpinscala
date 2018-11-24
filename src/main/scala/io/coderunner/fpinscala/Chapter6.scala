package io.coderunner.fpinscala

import scala.annotation.tailrec

trait Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  // A Linear congruential generator
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object ex6p1 extends Example {

    val name = "Ex6.1 - Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue"

    def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      case (r, rng) if r < 0 => Math.abs(r + 1) -> rng
      case (r, rng) => Math.abs(r) -> rng
    }
  }

  object ex6p2 extends Example {

    val name = "Ex6.2 - Write a function to generate a Double between 0 and 1, not including 1"

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = ex6p1.nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }
  }

  object ex6p3 extends Example {

    import ex6p1._
    import ex6p2._

    val name = "Ex6.3 - Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple"

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = rng.nextInt
      val (i2, r2) = double(r)
      ((i, i2), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, r) = double(rng)
      val (i2, r2) = nonNegativeInt(r)
      ((d, i2), r2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r) = double(rng)
      val (d2, r2) = double(r)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }
  }


}