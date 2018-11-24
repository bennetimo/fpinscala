package io.coderunner.fpinscala

import org.scalacheck.Gen._

class Chapter6Spec extends UnitTest with Chapter6 {

  behavior of s"${ex6p1.name}"
  it should "generate a random number between 0 and Int.maxValue" in {
    forAll((n: Int) => {
      ex6p1.nonNegativeInt(SimpleRNG(n))._1 should (be >= 0 and be < Int.MaxValue)
    })
  }

  behavior of s"${ex6p2.name}"
  it should "generate a random double between 0 and 1 not inclusive" in {
    forAll((n: Int) => {
      ex6p2.double(SimpleRNG(n))._1 should (be >= 0.0 and be < 1.0)
    })
  }

}
