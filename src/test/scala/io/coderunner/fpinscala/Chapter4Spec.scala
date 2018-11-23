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

}
