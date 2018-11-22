package io.coderunner.fpinscala

import scala.annotation.tailrec

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
      case Cons(_, t) => t
    }
  }

  object ex3p3 extends Example {

    val name = "Ex3.3 - Implement setHead for replacing the first element of a List with a different value"

    def setHead[A](newHead: A, l: List[A]): List[A] = l match {
      case Nil => sys.error("setHead on empty List")
      case Cons(_, t) => Cons(newHead, t)
    }
  }

  object ex3p4 extends Example {

    val name = "Ex3.4 - Generalise tail to the function drop, which removes the first n elements from a List"

    def drop[A](n: Int, l: List[A]): List[A] = {
      if (n <= 0) l
      else drop(n-1, ex3p2.tail(l))
    }
  }

  object ex3p5 extends Example {

    val name = "Ex3.5 - Implement dropWhile, which removes elements from the List prefix as long as they match a predicate"

    @tailrec
    def dropWhile[A](l: List[A])(f: (A => Boolean)): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if(f(h)) dropWhile(t)(f) else l
    }
  }

  object ex3p6 extends Example {

    val name = "Ex3.6 - Implement init that returns a List consisting of all but the last element of a List"

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  object ex3p7 extends Example {

    val name = "Ex3.7 - Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters 0.0?"
    //No, because foldRight starts at the far right of the list and has to unwind all the recursive calls. The recursive call will
    //be made first, so that even if there is a 0.0 somewhere in the list it will not be looked at until the list starts to unwind

    def foldRight[A, B](as: List[A], z: B)( f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
    }
  }

  object ex3p8 extends Example {

    val name = "Ex3.8 - What happens when you pass Nil and Cons themselves to foldRight?"

    /*
    The list is passed through once and reconstructed, the output being the exact same list
    foldRight(List(1, 2, 3), Nil)( Cons(_, _))
    Cons(1, foldRight(List(2, 3), Nil)( Cons(_, _)))
    Cons(1, Cons(2, foldRight(List(3), Nil))( Cons(_, _)))
    Cons(1, Cons(2, Cons(3, foldRight(List(Nil), Nil)))))( Cons(_, _))
    Cons(1, Cons(2, Cons(3, Nil)))( Cons(_, _))
     */
  }

}