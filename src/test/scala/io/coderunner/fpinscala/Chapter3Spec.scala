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
  it should "return throw an error if the original list is Nil" in {
    forAll((n: Int) => {
      an [RuntimeException] should be thrownBy ex3p3.setHead(n, Nil)
    })
  }

  it should "return Cons(new, t)) if the original list is not Nil" in {
    forAll((n: Int, n2: Int, n3: Int) => {
      ex3p3.setHead(n, List(n2, n3)) should be(Cons(n, Cons(n3, Nil)))
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

  behavior of s"${ex3p5.name}"
  it should "keep dropping until predicate match" in {
    ex3p5.dropWhile(List(1, 2, -1, 3))(x => x > 0) should be(Cons(-1, Cons(3, Nil)))
  }

  behavior of s"${ex3p6.name}"
  it should "return Nil for a list of one element" in {
    ex3p6.init(List(1)) should be(Nil)
  }
  it should "chop of the last element in the List" in {
    ex3p6.init(List(1, 2, 3, 4)) should be(List(1,2,3))
  }

  behavior of s"${ex3p7.name}"
  it should "not be able to short-circuit when calculating product if 0.0 is found at the end (right)" in {
    ex3p7.foldRight(List(1, 2, 3, 4, 0), (1, 0))( (x, acc) => (x * acc._1, acc._2 + 1)) should be((0, 5))
  }
  it should "not be able to short-circuit when calculating product if 0.0 is found at the start (left)" in {
    ex3p7.foldRight(List(0, 1, 2, 3, 4), (1, 0))( (x, acc) => (x * acc._1, acc._2 + 1)) should be((0, 5))
  }
  it should "work for sum when using foldRight" in {
    ex3p7.foldRight(List(1, 2, 3, 4), 0)( (x,y) => x+y) should be(10)
  }
  it should "work for product when using foldRight" in {
    ex3p7.foldRight(List(1, 2, 3, 4), 1)( (x,y) => x*y) should be(24)
  }

  behavior of s"${ex3p8.name}"
  it should "give us back the original list" in {
    ex3p7.foldRight(List(1, 2, 3), Nil:List[Int])( Cons(_, _)) should be(List(1, 2, 3))
  }

  behavior of s"${ex3p9.name}"
  it should "return length 0 for the empty list" in {
    ex3p9.length(Nil) should be(0)
  }
  it should "return length the appropriate length for a non-empty list" in {
    forAll(nonEmptyListOf(posNum[Int])){ ints =>
      ex3p9.length(List(ints :_*)) should be(ints.length)
    }
  }

  behavior of s"${ex3p10.name}"
  it should "throw a StackOverflowError for large lists using foldRight" in {
    val scalaList = (1 to 10000).toList
    val ourList = List.applyIterative(scalaList :_*)
    an [StackOverflowError] should be thrownBy ex3p7.foldRight(ourList, "")((x, y) => "X" + x)
  }
  it should "not throw a StackOverflowError for large lists using foldLeft" in {
    val scalaList = (10001 to 35000).toList
    val ourList = List.applyIterative(scalaList :_*)
    noException should be thrownBy ex3p10.foldLeft(ourList, "")((x, y) => "X" + x)
  }

  behavior of s"${ex3p11.name}"
  it should "return the correct sum of a list" in {
    ex3p11.sum(List(1,2,3,4,5)) should be(15)
  }
  it should "return the correct product of a list" in {
    ex3p11.product(List(1,2,3,4,5)) should be(120)
  }
  it should "return the correct length of a list" in {
    ex3p11.length(List(1,2,3,4,5)) should be(5)
  }

  behavior of s"${ex3p12.name}"
  it should "reverse a list" in {
    ex3p12.reverse(List(1,2,3,4)) should be(List(4,3,2,1))
  }

  behavior of s"${ex3p13.name}"
  it should "implement foldRight via foldLeft, so that folding a List with the type constructors gives us back the original list" in {
    ex3p13.foldRightViaFL(List(1, 2, 3), Nil:List[Int])( Cons(_, _)) should be(List(1, 2, 3))
  }
  it should "implement foldLeft via foldRight, so that folding a List with the type constructors gives us back a reversed list" in {
    ex3p13.foldLeftViaFoldRight(List(1, 2, 3), Nil:List[Int])( Cons(_, _)) should be(List(3, 2, 1))
  }

  behavior of s"${ex3p14.name}"
  it should "append an element to the end of the list" in {
    ex3p14.append(List(1, 2, 3), List(4)) should be(List(1, 2, 3, 4))
  }

  behavior of s"${ex3p15.name}"
  it should "concat lists together" in {
    ex3p15.concat(List(List(1), List(2), List(3))) should be(List(1, 2, 3))
  }
  it should "concat lists together when implemented using foldRight" in {
    ex3p15.concatFR(List(List(1), List(2), List(3))) should be(List(1, 2, 3))
  }

  behavior of s"${ex3p16.name}"
  it should "add one to each element" in {
    ex3p16.addOne(List(1, 2, 3)) should be(List(2, 3, 4))
  }

  behavior of s"${ex3p17.name}"
  it should "stringify a list of doubles" in {
    ex3p17.stringify(List(1.0, 2.0, 3.0)) should be(List("1.0", "2.0", "3.0"))
  }

  behavior of s"${ex3p18.name}"
  it should "be able to map all elements + 2" in {
    ex3p18.map(List(1, 2, 3))(_ + 2) should be(List(3, 4, 5))
  }
  it should "be able to map all elements + 2 in a stack safe way" in {
    ex3p18.mapStackSafe(List(1, 2, 3))(_ + 2) should be(List(3, 4, 5))
  }

  behavior of s"${ex3p19.name}"
  it should "filter all odd numbers from a list" in {
    ex3p19.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(List(2, 4))
  }

  behavior of s"${ex3p20.name}"
  it should "map and then flatten (via foldRight and append)" in {
    ex3p20.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }
  it should "map and then flatten (via concat and map)" in {
    ex3p20.flatMapViaConcat(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  behavior of s"${ex3p21.name}"
  it should "filter all odd numbers from a list (implemented via flatMap)" in {
    ex3p21.filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(List(2, 4))
  }

  behavior of s"${ex3p22.name}"
  it should "combine and add items at the same index across two lists" in {
    ex3p22.combineAdd(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  behavior of s"${ex3p23.name}"
  it should "add corresponding items if zipWith (_ + _)" in {
    ex3p23.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
  }
  it should "concat letters if zipWith (_ + _) for strings" in {
    ex3p23.zipWith(List("a", "b", "c"), List(1, 2, 3))((a, b) => a + b) should be(List("a1", "b2", "c3"))
  }

  behavior of s"${ex3p24.name}"
  it should "find List(2, 3) as a subsequence of List(1, 2, 3, 4)" in {
    ex3p24.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be(true)
  }
  it should "find Nil as a subsequence of List(1, 2, 3, 4)" in {
    ex3p24.hasSubsequence(List(1, 2, 3, 4), Nil) should be(true)
  }
  it should "find not find List(2, 4) as a subsequence of List(1, 2, 3, 4)" in {
    ex3p24.hasSubsequence(List(1, 2, 3, 4), List(2, 4)) should be(false)
  }

  behavior of s"${ex3p25.name}"
  it should "return one for a Tree of a single leaf" in {
    ex3p25.size(Leaf(1)) should be(1)
  }
  it should "return three for a Tree of a single branch" in {
    ex3p25.size(Branch(Leaf(1), Leaf(2))) should be(3)
  }
  it should "return the size of a more complex tree" in {
    ex3p25.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) should be(7)
  }

  behavior of s"${ex3p26.name}"
  it should "return the max value in a tree" in {
    ex3p26.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(8), Leaf(4)))) should be(8)
  }

}
