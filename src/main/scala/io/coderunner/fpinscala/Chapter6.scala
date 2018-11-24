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


}