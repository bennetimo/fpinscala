package io.coderunner.fpinscala

import org.scalacheck.Gen._

class Chapter6Spec extends UnitTest with Chapter6 {

  behavior of s"${ex6p1.name}"
  it should "generate a random number between 0 and Int.maxValue" in {
    forAll((n: Int) => {
      ex6p1.nonNegativeInt(SimpleRNG(n))._1 should (be >= 0 and be < Int.MaxValue)
    })
  }

}
