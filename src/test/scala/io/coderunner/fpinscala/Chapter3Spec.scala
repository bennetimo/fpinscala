package io.coderunner.fpinscala

class Chapter3Spec extends UnitTest with Chapter3 {

  s"${ex3p1.name}" should "match the third case" in {
    ex3p1.m should be(3)
  }

}
