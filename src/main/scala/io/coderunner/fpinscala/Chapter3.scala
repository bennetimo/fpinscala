package io.coderunner.fpinscala

trait Chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object ex3p1 extends Example {

    val name = "Ex3.1 - What will be the result of the following match expression?"

    def m: Any = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  object ex3p2 extends Example {

    val name = "Ex3.2 - Implement the function tail for removing the first element of a List"

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  object ex3p3 extends Example {

    val name = "Ex3.3 - Implement setHead for replacing the first element of a List with a different value"

    def setHead[A](newHead: A, l: List[A]): List[A] = l match {
      case Nil => Cons(newHead, Nil)
      case Cons(h, t) => Cons(newHead, Cons(h, t))
    }
  }

  object ex3p4 extends Example {

    val name = "Ex3.4 - Generalise tail to the function drop, which removes the first n elements from a List"

    def drop[A](n: Int, l: List[A]): List[A] = {
      if (n <= 0) l
      else drop(n-1, ex3p2.tail(l))
    }
  }

}