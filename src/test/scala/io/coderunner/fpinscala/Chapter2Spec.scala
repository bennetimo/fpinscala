package io.coderunner.fpinscala

import org.scalacheck._
import org.scalacheck.Gen._

import scala.reflect.ClassTag

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

  s"${ex2p2.name} (Attempt #1: isSorted[Int])" should behave like isSortedChecks[Int](ex2p2.isSorted)
  s"${ex2p2.name} (Attempt #1: isSorted[String])" should behave like isSortedChecks[String](ex2p2.isSorted)
  s"${ex2p2.name} (Attempt #2 (non recursive): isSorted2[Int])" should behave like isSortedChecks[Int](ex2p2.isSorted2)
  s"${ex2p2.name} (Attempt #2 (non recursive): isSorted2[String])" should behave like isSortedChecks[String](ex2p2.isSorted2)
  s"${ex2p2.name} (Attempt #3 (recursive simpler): isSorted3[Int])" should behave like isSortedChecks[Int](ex2p2.isSorted3)
  s"${ex2p2.name} (Attempt #3 (recursive simpler): isSorted3[String])" should behave like isSortedChecks[String](ex2p2.isSorted3)

  behavior of s"${ex2p3.name}"

  it should "return a new function that partially applies the first argument" in {
    val f: (String, Int) => String = (s: String, i: Int) => s"$s-$i"
    val curried = ex2p3.curry(f)
    curried("hello")(5) should be(f("hello", 5))
  }

  behavior of s"${ex2p4.name}"

  it should "return a new function that uncurries the arguments" in {
    val f: String => Int => String = (s: String) => (i: Int) => s"$s-$i"

    val uncurried = ex2p4.uncurry(f)
    uncurried("hello", 5) should be(f("hello")(5))
  }

  behavior of s"${ex2p5.name}"

  it should "return a new function that composes the two passed in functions" in {
    val f1: Int => Int = i => i+10
    val f2: Int => Int = i => i*2

    forAll(posNum[Int]){ n =>
      ex2p5.compose(f1, f2)(n) should be((n*2)+10)
    }
  }

}

trait Chapter2Behaviours { this: UnitTest =>

  def fibChecks(fn: => Int => Int): Unit = {

    it should "return 0 for all negative values" in {
      forAll(negNum[Int]){
        n => fn(n) should be(0)
      }
    }

    it should "return 0 for fib(1)" in {
      fn(1) should be(0)
    }

    it should "return 1 for fib(2)" in {
      fn(2) should be(1)
    }

    it should "return 1 for fib(3)" in {
      fn(3) should be(1)
    }

    it should "return 2 for fib(4)" in {
      fn(4) should be(2)
    }

    it should "return 34 for fib(10)" in {
      fn(10) should be(34)
    }

    it should "return the first 10 numbers in the sequence" in {
      (1 to 10).map(fn) should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }

  }

  def isSortedChecks[A <% Ordered[A] : ClassTag](fn: => (Array[A], (A,A) => Boolean) => Boolean)(implicit a: Arbitrary[A]): Unit = {

    it should "return true for an array of zero items" in {
      fn(Array(), (x, y) => x <= y) should be(true)
    }

    it should "return true for an array of one item" in {
      forAll( (n: A) => {
        fn(Array(n), (x, y) => x <= y) should be(true)
      })
    }

    it should "return true for an array of two items in order" in {
      forAll( (n1: A, n2: A) => {
        whenever(n2 > n1) {
          fn(Array(n1, n2), (x, y) => x <= y) should be(true)
        }
      })
    }

    it should "return true for an array of two identical items" in {
      forAll( (n1: A) => {
        fn(Array(n1, n1), (x, y) => x <= y) should be(true)
      })
    }

    it should "return false for an array of two items in the wrong order" in {
      forAll( (n1: A, n2: A) => {
        whenever(n2 < n1) {
          fn(Array(n1, n2), (x, y) => x <= y) should be(false)
        }
      })
    }

    it should "return false for an array of two items in order and the rest with additional unordered items" in {
      forAll( (n1: A, n2: A, n3: A, n4: A, n5: A) => {
        whenever(n2 > n1) {
          fn(Array(n2, n3, n1, n2, n4, n5), (x, y) => x <= y) should be(false)
        }
      })
    }

  }
}