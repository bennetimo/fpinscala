package io.coderunner.fpinscala

class Chapter5Spec extends UnitTest with Chapter5 {

  behavior of s"${ex5p1.name}"
  it should "convert a Stream to a List by evaluating it" in {
    Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
  }
  it should "convert a Stream to a List by evaluating it (tail recursive)" in {
    Stream(1, 2, 3, 4).toListTR should be(List(1, 2, 3, 4))
  }

  behavior of s"${ex5p2.name}"
  it should "return the first values of a Stream (take)" in {
    Stream(1, 2, 3, 4).take(2).toList should be(List(1, 2))
  }
  it should "return a new Stream without the first values (drop)" in {
    Stream(1, 2, 3, 4).drop(2).toList should be(List(3, 4))
  }

  behavior of s"${ex5p3.name}"
  it should "return all the starting values matching the predicate" in {
    Stream(1, 5, 2).takeWhile(_ % 2 != 0).toList should be(List(1, 5))
  }

  behavior of s"${ex5p4.name}"
  it should "return false if one value in the stream doesn't satisfy the predicate" in {
    Stream(2, 4, 5).forAll(_ % 2 == 0) should be(false)
  }
  it should "return true if all values in the stream satisfy the predicate" in {
    Stream(2, 4, 5).forAll(_ >= 2) should be(true)
  }
  it should "return false if one value in the stream doesn't satisfy the predicate (via foldRight)" in {
    Stream(2, 4, 5).forAllViaFR(_ % 2 == 0) should be(false)
  }
  it should "return true if all values in the stream satisfy the predicate (via foldRight)" in {
    Stream(2, 4, 5).forAllViaFR(_ >= 2) should be(true)
  }

  behavior of s"${ex5p5.name}"
  it should "return all the starting values matching the predicate" in {
    Stream(1, 5, 2).takeWhileViaFR(_ % 2 != 0).toList should be(List(1, 5))
  }

  behavior of s"${ex5p6.name}"
  it should "return None for an empty stream" in {
    Stream().headOption should be(None)
  }
  it should "return Some(head) for a non-empty stream" in {
    Stream(1, 2).headOption should be(Some(1))
  }

}
