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

  behavior of s"${ex6p3.name}"
  it should "generate a random (Int, Double) pair" in {
    forAll((n: Int) => {
      ex6p3.intDouble(SimpleRNG(n))._1._1 should (be >= Int.MinValue and be < Int.MaxValue)
      ex6p3.intDouble(SimpleRNG(n))._1._2 should (be >= 0.0 and be < 1.0)
    })
  }
  it should "generate a random (Double, Int) pair" in {
    forAll((n: Int) => {
      ex6p3.doubleInt(SimpleRNG(n))._1._2 should (be >= Int.MinValue and be < Int.MaxValue)
      ex6p3.doubleInt(SimpleRNG(n))._1._1 should (be >= 0.0 and be < 1.0)
    })
  }
  it should "generate a random (Double, Double, Double) 3-tuple" in {
    forAll((n: Int) => {
      val (d1, d2, d3) = ex6p3.double3(SimpleRNG(n))._1
      d1 should not (be(d2) and not be(d3))
    })
  }

}
