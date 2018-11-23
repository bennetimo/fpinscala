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

    // Taken from book
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {// The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(h, t) =>
        if(p(h())) t().forAll(p)
        else false
    }

    def forAllViaFR(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def takeWhileViaFR(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((elem, acc) => if(p(elem)) Stream.cons(elem, acc) else Stream.empty)

    def headOption: Option[A] = foldRight(None:Option[A])((elem, _) => Some(elem))

    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])( (elem, acc) => Stream.cons(f(elem), acc))

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])( (elem, acc) => if(f(elem)) Stream.cons(elem, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)( (a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])( (a, b) => f(a).append(b))
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

  object ex5p4 extends Example {

    val name = "Ex5.4 - Implement forAll which checks that all elements in the Stream match a given predicate"
  }

  object ex5p5 extends Example {

    val name = "Ex5.5 - Use foldRight to implement takeWhile"
  }

  object ex5p6 extends Example {

    val name = "Ex5.6 (Hard) - Implement headOption via foldRight"
  }

  object ex5p7 extends Example {

    val name = "Ex5.7 - Implement map, filter, append and flatMap using foldRight"
  }

  object ex5p8 extends Example {

    val name = "Ex5.8 - Generalize ones slightly to the function constant, which returns an infinite Stream of a given value"

    def constant[A](value: A): Stream[A] = Stream.cons(value, constant(value))
  }

  object ex5p9 extends Example {

    val name = "Ex5.9 - Write a function that generates an infinite stream of integers, n, n+1, n+2 etc"

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  }

}