package io.coderunner.fpinscala

import org.scalacheck._
import org.scalacheck.Gen._

class Chapter3Spec extends UnitTest with Chapter3 {

  s"${ex3p1.name}" should "match the third case" in {
    ex3p1.m should be(3)
  }

  behavior of s"${ex3p2.name}"
  it should "return Nil when called with Nil" in {
    ex3p2.tail(Nil) should be(Nil)
  }

  it should "return Nil for a list of one element" in {
    forAll((n: Int) => {
      ex3p2.tail(List(n)) should be(Nil)
    })
  }

  it should "return Cons(t, Nil) for a list of two elements" in {
    forAll((n: Int, n2: Int) => {
      ex3p2.tail(List(n, n2)) should be(Cons(n2, Nil))
    })
  }

  it should "return the tail for a list with multiple elements" in {
    forAll(listOfN(5, posNum[Int])) { l =>
      val scala.List(n1, n2, n3, n4, n5) = l

      ex3p2.tail(List.apply(l :_*)) should be(Cons(n2, Cons(n3, Cons(n4, Cons(n5, Nil)))))
    }
  }

}
