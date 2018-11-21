package io.coderunner.fpinscala

import org.scalacheck.Gen._

class Chapter2Spec extends UnitTest with Chapter2 with Chapter2Behaviours {

  s"${ex2p1.name} (Attempt #1: fib() returning a single number)" should behave like fibChecks(ex2p1.fib)

  behavior of s"${ex2p1.name} (Attempt #2: fibAll() returning all numbers up to n)"

  it should "return Nil for all negative values" in {
    forAll(negNum[Int]) {
      n => ex2p1.fibAll(n) should be(Nil)
    }
  }

  it should "return List(0) for fibAll(1)" in {
    ex2p1.fibAll(1) should be(List(0))
  }

  it should "return List(0,1) for fibAll(2)" in {
    ex2p1.fibAll(2) should be(List(0, 1))
  }

  it should "return the first 10 numbers in the sequence" in {
    ex2p1.fibAll(10) should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  s"${ex2p1.name} (Attempt #3: fibNotTR() returning a single number)" should behave like fibChecks(ex2p1.fibNotTR)

  s"${ex2p1.name} (Attempt #4: fib2() returning a single number)" should behave like fibChecks(ex2p1.fib2)

}

trait Chapter2Behaviours { this: UnitTest =>

  def fibChecks(fibFn: => Int => Int): Unit = {

    it should "return 0 for all negative values" in {
      forAll(negNum[Int]){
        n => fibFn(n) should be(0)
      }
    }

    it should "return 0 for fib(1)" in {
      fibFn(1) should be(0)
    }

    it should "return 1 for fib(2)" in {
      fibFn(2) should be(1)
    }

    it should "return 1 for fib(3)" in {
      fibFn(3) should be(1)
    }

    it should "return 2 for fib(4)" in {
      fibFn(4) should be(2)
    }

    it should "return 34 for fib(10)" in {
      fibFn(10) should be(34)
    }

    it should "return the first 10 numbers in the sequence" in {
      (1 to 10).map(fibFn) should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }

  }
}