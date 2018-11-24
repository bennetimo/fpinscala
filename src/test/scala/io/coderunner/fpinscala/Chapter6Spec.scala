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

  behavior of s"${ex6p9.name}"
  it should "map the random number generator (map via flatMap)" in {
    forAll((n: Int) => {
      import ex6p5.double
      ex6p9.map(double)((a) => a * 2)(SimpleRNG(n))._1 should (be >= 0.0 and be < 2.0)
    })
  }
  it should "apply the combiner function to the two states (map2 via flatMap)" in {
    forAll((n: Int) => {
      import ex6p5.double
      ex6p9.map2(double,double)((a,b) => a + b)(SimpleRNG(n))._1 should (be >= 0.0 and be < 2.0)
    })
  }

  behavior of s"${ex6p10.name}"
  it should "create a state with a value (unit via State)" in {
    forAll((n: Int) => {
      State.unit(10).run(SimpleRNG(n))._1 should be(10)
    })
  }
  it should "map the random number generator (map via State)" in {
    forAll((n: Int) => {
      val s = State[RNG, Double](s => (1.0, s))
      s.map(a => a * 2).run(SimpleRNG(n))._1 should (be >= 0.0 and be <= 2.0)
    })
  }
  it should "apply the combiner function to the two states (map2 via State)" in {
    forAll((n: Int) => {
      val s = State[RNG, Double](s => (1.0, s))
      val s2 = State[RNG, Double](s => (2.0, s))
      s.map2(s2)((a, b) => a + b).run(SimpleRNG(n))._1 should be(3.0)
    })
  }
  it should "map and flatten the random number generator (flatMap via State)" in {
    forAll((n: Int) => {
      val s = State[RNG, Double](s => (1.0, s))
      s.flatMap(a => State.unit(a * 2)).run(SimpleRNG(n))._1 should be(2.0)
    })
  }
  it should "sequence together random generators (sequence via State)" in {
    forAll((n: Int) => {
      val s = State[RNG, Double](s => (1.0, s))
      val s2 = State[RNG, Double](s => (2.0, s))
      val s3 = State[RNG, Double](s => (3.0, s))

      State.sequenceViaFoldRight(List(s, s2, s3)).run(SimpleRNG(n))._1 should be(List(1.0, 2.0, 3.0))
    })
  }

  behavior of s"${ex6p11.name}"
  it should "return the number of coins (14) and candies (1 left) after it is simulated" in {
    val machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn)

    Machine.simulateMachine(inputs).run(machine)._1 should be (14, 1)
  }

}
