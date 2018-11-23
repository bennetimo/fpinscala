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

  behavior of s"${ex5p7.name}"
  it should "apply f to all the items in the Stream (map)" in {
    Stream(1, 2, 3).map(_ * 2).toList should be(List(2, 4, 6))
  }
  it should "filter out items not matching the predicate (filter)" in {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList should be(List(2, 4))
  }
  it should "add a new item to the end of the stream (append)" in {
    Stream(1, 2, 3).append(Stream(4)).toList should be(List(1, 2, 3, 4))
  }
  it should "apply f to all items and flatten (flatMap)" in {
    Stream(1, 2, 3).flatMap(a => Stream.cons(a * 2, Empty)).toList should be(List(2, 4, 6))
  }

  behavior of s"${ex5p8.name}"
  it should "generate an infinite stream of constant values" in {
    ex5p8.constant(2).take(3).toList should be(List(2, 2, 2))
  }

  behavior of s"${ex5p9.name}"
  it should "generate an infinite stream of increasing ints" in {
    ex5p9.from(5).take(4).toList should be(List(5, 6, 7, 8))
  }

  behavior of s"${ex5p10.name}"
  it should "generate an infinite stream of fibonacci numbers" in {
    ex5p10.fibs.take(8).toList should be(List(0,1,1,2,3,5,8,13))
  }

  behavior of s"${ex5p11.name}"
  it should "generate an infinite stream of fibonacci numbers" in {
    ex5p11.unfold(3)(i => Some(i, i+1)).take(3).toList should be(List(3, 4, 5))
  }

  behavior of s"${ex5p12.name}"
  it should "generate an infinite stream of fibonacci numbers (via unfold)" in {
    ex5p12.fibs.take(8).toList should be(List(0,1,1,2,3,5,8,13))
  }
  it should "generate an infinite stream of increasing ints (via unfold)" in {
    ex5p12.from(5).take(4).toList should be(List(5, 6, 7, 8))
  }
  it should "generate an infinite stream of constant values (via unfold)" in {
    ex5p12.constant(2).take(3).toList should be(List(2, 2, 2))
  }
  it should "generate an infinite stream of 1s (via unfold)" in {
    ex5p12.ones.take(3).toList should be(List(1, 1, 1))
  }

  behavior of s"${ex5p13.name}"
  it should "apply f to all the items in the Stream (mapViaUnfold)" in {
    Stream(1, 2, 3).mapViaUnfold(_ * 2).toList should be(List(2, 4, 6))
  }
  it should "return the first values of a Stream (takeViaUnfold)" in {
    Stream(1, 2, 3, 4).takeViaUnfold(2).toList should be(List(1, 2))
  }
  it should "return all the starting values matching the predicate (takeWhileViaUnfold)" in {
    Stream(1, 5, 2).takeWhileViaUnfold(_ % 2 != 0).toList should be(List(1, 5))
  }
  it should "combine the elements of the two streams one by one" in {
    Stream(1, 2, 3).zipWith(Stream("a", "b", "c"))( (a, b) => s"$a$b").toList should be(List("1a", "2b", "3c"))
  }
  it should "combine the elements of the two streams one by one as long as either stream has more elements" in {
    Stream(1, 2, 3).zipAll(Stream("a")).toList should be(List(Some(1) -> Some("a"), Some(2) -> None, Some(3) -> None))
  }

}
