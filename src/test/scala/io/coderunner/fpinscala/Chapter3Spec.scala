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

  behavior of s"${ex3p3.name}"
  it should "return Cons(new, Nil) if the original list is Nil" in {
    forAll((n: Int) => {
      ex3p3.setHead(n, Nil) should be(Cons(n, Nil))
    })
  }

  it should "return Cons(new, Cons(h, t)) if the original list is not Nil" in {
    forAll((n: Int, n2: Int, n3: Int) => {
      ex3p3.setHead(n, List(n2, n3)) should be(Cons(n, Cons(n2, Cons(n3, Nil))))
    })
  }

  behavior of s"${ex3p4.name}"
  it should "return the tail if n is 1" in {
    ex3p4.drop(1, List(1,2,3)) should be(Cons(2, Cons(3, Nil)))
  }

  it should "return the tail tail if n is 2" in {
    ex3p4.drop(2, List(1,2,3)) should be(Cons(3, Nil))
  }

  it should "return Nil if n >= list.length" in {
    ex3p4.drop(3, List(1,2,3)) should be(Nil)
  }

}
