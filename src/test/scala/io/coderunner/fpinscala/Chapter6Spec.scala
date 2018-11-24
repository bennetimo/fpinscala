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

  behavior of s"${ex6p4.name}"
  it should "generate a list of random numbers" in {
    forAll((n: Int) => {
      val ints = ex6p4.ints(5)(SimpleRNG(n))._1
      ints.toSet.size should be(5)
    })
  }

  behavior of s"${ex6p5.name}"
  it should "generate a random double between 0 and 1 not inclusive" in {
    forAll((n: Int) => {
      ex6p5.double(SimpleRNG(n))._1 should (be >= 0.0 and be < 1.0)
    })
  }

  behavior of s"${ex6p6.name}"
  it should "apply the combiner function to the two states" in {
    forAll((n: Int) => {
      import ex6p5.double
      ex6p6.map2(double,double)((a,b) => a + b)(SimpleRNG(n))._1 should (be >= 0.0 and be < 2.0)
    })
  }

  behavior of s"${ex6p7.name}"
  it should "sequence together random generators" in {
    forAll((n: Int) => {
      import ex6p5.double
      val l = List(double, double, double)
      ex6p7.sequence(l)(SimpleRNG(n))._1.size should be(3)
    })
  }

  behavior of s"${ex6p8.name}"
  it should "generate positive ints less than n" in {
    forAll((n: Int) => {
      ex6p8.nonNegativeLessThan(10)(SimpleRNG(n))._1 should be <= 10
    })
  }

}
