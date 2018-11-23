package io.coderunner.fpinscala

import org.scalacheck.Gen._

class Chapter4Spec extends UnitTest with Chapter4 {

  behavior of s"${ex4p1.name}"
  it should "return None for a None (map)" in {
    (None:Option[Int]).map(_+10) should be(None)
  }
  it should "apply f to a Some (map)" in {
    Some(5).map(_+10) should be(Some(15))
  }
  it should "return None for a None (flatMap)" in {
    (None:Option[Int]).flatMap((a) => Some(a+10)) should be(None)
  }
  it should "return apply f to the inner value for a Some (flatMap)" in {
    Some(5).flatMap((a) => Some(a+10)) should be(Some(15))
  }
  it should "return the default for a None (getOrElse)" in {
    (None:Option[Int]).getOrElse(42) should be(42)
  }
  it should "return the value for a Some (getOrElse)" in {
    Some(12).getOrElse(42) should be(12)
  }
  it should "return the second option for a None (orElse)" in {
    (None:Option[Int]).orElse(Some(42)) should be(Some(42))
  }
  it should "return the first option for a Some (orElse)" in {
    (Some(12)).orElse(Some(42)) should be(Some(12))
  }
  it should "return None for a None filtered with anything (filter)" in {
    (None:Option[Int]).filter(_>= 42) should be(None)
  }
  it should "return None for a Some that doesn't match the filter" in {
    Some(23).filter(_>= 42) should be(None)
  }
  it should "return the same Some if it matches the match the filter" in {
    Some(52).filter(_>= 42) should be(Some(52))
  }

  behavior of s"${ex4p2.name}"
  it should "return the variance of a Seq" in {
    ex4p2.variance(List(1,3,2,5)) should be(Some(2.1875))
  }

  behavior of s"${ex4p3.name}"
  it should "combine two option values with +" in {
    ex4p3.map2(Some(5), Some(2))(_ + _) should be(Some(7))
  }

  behavior of s"${ex4p4.name}"
  it should "return None if any of the options are None" in {
    ex4p4.sequence(List(Some(1), None, Some(3))) should be(None)
  }
  it should "return Some if all the options are Some" in {
    ex4p4.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  behavior of s"${ex4p5.name}"
  def dummyFn(i: Int): Option[String] = if(i < 10) Some(i.toString) else None
  it should "return None if the fn ever results in a None" in {
    ex4p5.traverse(List(1,2,12,3))(dummyFn) should be(None)
  }
  it should "return Some if the fn always gives a Some" in {
    ex4p5.traverse(List(1,2,3,4))(dummyFn) should be(Some(List("1","2","3","4")))
  }
  it should "return None if any of the options are None (sequenceViaTraverse)" in {
    ex4p5.sequenceViaTraverse(List(Some(1), None, Some(3))) should be(None)
  }
  it should "return Some if all the options are Some (sequenceViaTraverse)" in {
    ex4p5.sequenceViaTraverse(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  behavior of s"${ex4p6.name}"
  it should "return a Left when mapping a Left value (map)" in {
    (Left("error"):Either[String, Int]).map(_ + 1) should be(Left("error"))
  }
  it should "return a Right with the fn applied when mapping a Right value (map)" in {
    (Right(10):Either[String, Int]).map(_ + 1) should be(Right(11))
  }
  it should "return a Left when flatMapping a Left value (flatMap)" in {
    (Left("error"):Either[String, Int]).flatMap(a => Right(a + 1)) should be(Left("error"))
  }
  it should "return a Right when flatMapping a Right value (flatMap)" in {
    (Right(10):Either[String, Int]).flatMap(a => Right(a + 1)) should be(Right(11))
  }
  it should "return the second Either when the first is a Left (orElse)" in {
    (Left("error"):Either[String, Int]).orElse(Right(12)) should be(Right(12))
  }
  it should "return the second Either (even if it is left) when the first is a Left (orElse)" in {
    (Left("error"):Either[String, Int]).orElse(Left("also error")) should be(Left("also error"))
  }
  it should "apply the function to the values inside each Right (first is left) (map2)" in {
    (Left("error"):Either[String, Int]).map2(Left("also error"):Either[String, Int])(_ * _) should be(Left("error"))
  }
  it should "apply the function to the values inside each Right (second is left) (map2)" in {
    (Right(12):Either[String, Int]).map2(Left("also error"):Either[String, Int])(_ * _) should be(Left("also error"))
  }
  it should "apply the function to the values inside each Right (both right) (map2)" in {
    (Right(12):Either[String, Int]).map2(Right(2):Either[String, Int])(_ * _) should be(Right(24))
  }

  behavior of s"${ex4p7.name}"
  it should "return the first Left if any of the list is Left (sequence)" in {
    ex4p7.sequence(List(Right(4), Right(6), Left("error"), Right(2))) should be(Left("error"))
  }
  it should "return the first Left if any of the list is Left (multiple lefts) (sequence)" in {
    ex4p7.sequence(List(Right(4), Right(6), Left("error"), Left("error2"), Right(2))) should be(Left("error"))
  }
  it should "return a Right with all the values as a list if all are Right (sequence)" in {
    ex4p7.sequence(List(Right(4), Right(6), Right(2))) should be(Right(List(4, 6, 2)))
  }
  def dummyFnTraverse(i: Int): Either[String, Int] = if(i < 10) Right(i * 2) else Left("nope!")
  it should "return the first Left if any of the list when the function is applied is Left (traverse)" in {
    ex4p7.traverse(List(1,4,12,2))(dummyFnTraverse) should be(Left("nope!"))
  }
  it should "return the list with all functions applied in a Right (traverse)" in {
    ex4p7.traverse(List(1,4,2))(dummyFnTraverse) should be(Right(List(2,8,4)))
  }

  behavior of s"${ex4p8.name}"

}
