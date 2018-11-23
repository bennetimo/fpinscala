package io.coderunner.fpinscala

import scala.annotation.tailrec

trait Chapter5 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def toListTR: List[A] = {
      @tailrec
      def loop(acc: List[A], remaining: Stream[A]): List[A] = remaining match {
        case Empty => acc
        case Cons(h, t) => loop(h() :: acc, t())
      }
      loop(Nil, this).reverse
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  object ex5p1 extends Example {

    val name = "Ex5.1 - Write a function to convert a Stream to a List"

  }

}