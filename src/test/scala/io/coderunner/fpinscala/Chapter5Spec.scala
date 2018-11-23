package io.coderunner.fpinscala

class Chapter5Spec extends UnitTest with Chapter5 {

  behavior of s"${ex5p1.name}"
  it should "convert a Stream to a List by evaluating it" in {
    Stream(1, 2, 3, 4).toList should be(List(1, 2, 3, 4))
  }
  it should "convert a Stream to a List by evaluating it (tail recursive)" in {
    Stream(1, 2, 3, 4).toListTR should be(List(1, 2, 3, 4))
  }

}
