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

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => Stream.cons(h(), Empty)
      case _ => Empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if(p(h())) Stream.cons(h(), t().takeWhile(p))
        else Empty
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

  object ex5p2 extends Example {

    val name = "Ex5.2 - Write the functions take(n) and drop(n) for a Stream"
  }

  object ex5p3 extends Example {

    val name = "Ex5.3 - Write the function takeWhile for returning all starting elements of a Stream that match the given predicate"
  }

}