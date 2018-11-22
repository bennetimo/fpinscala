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

    // Won't throw a StackOverflowError when applying a large number of varargs
    def applyIterative[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else {
        var l: List[A] = Nil
        var i = 0
        val reverse = as.reverse
        while(i < reverse.length){
          l = Cons(as(i), l)
          i = i + 1
        }
        l
      }
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

  object ex3p9 extends Example {

    val name = "Ex3.9 - Compute the length of a list using foldRight"

    def length[A](as: List[A]): Int = {
      ex3p7.foldRight(as, 0)((_, count) => count + 1)
    }

  }

  object ex3p10 extends Example {

    val name = "Ex3.10 - Convince yourself that foldRight is not stack-safe, and write foldLeft that is"

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)( f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(h, z))(f)
      }
    }

  }

  object ex3p11 extends Example {

    val name = "Ex3.11 - Write sum, product and a function to compute the length of a list using foldLeft"

    def sum(as: List[Int]): Int = ex3p10.foldLeft(as, 0)(_ + _)
    def product(as: List[Int]): Int = ex3p10.foldLeft(as, 1)(_ * _ )
    def length(as: List[Int]): Int = ex3p10.foldLeft(as, 0)( (_, count) => count + 1)
  }

  object ex3p12 extends Example {

    val name = "Ex3.12 - Write a function that returns the reverse of a list, using a fold"

    def reverse[A](as: List[A]): List[A] = ex3p10.foldLeft(as, Nil:List[A])( (a, acc) => Cons(a, acc))
  }

  object ex3p13 extends Example {

    val name = "Ex3.13 (Hard) - Can you write foldLeft in terms of foldRight, and vice-versa?"

    //This case is easy, we just reverse the list first, and then foldLeft over it
    def foldRightViaFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      val reversed = ex3p12.reverse(as)
      ex3p10.foldLeft(reversed, z)(f)
    }

    // This one is tricky. When we foldRight, we recurse all the way down to the far right of the list and then
    // start winding out. Whereas in a foldLeft, we want to start off with the left hand side and apply the f function
    // first there.
    // So in this case, we get all the way to the right and then for the last real element in the list we apply the
    // function f(h, foldRight(Nil, z)(f))) (from the definition of foldRight).
    // Our z value here is actually a function, that is a noop and just returns the passed in value.
    // So when we evaluate foldRight(Nil, z)(f) we get (b: B) => b
    // So moving one back out the recursion, towards the left, we have:
    // f(h, (b: B) => b)
    // Now what does the f function do? Given the current list item, a, and our current accumlated zero item (so far
    // just our noop function which will return the last element of the list), we return a new function which
    // composes our old one with a new one, this time applying f to the current element.
    // Basically we are wrapping up successive functions. The right most function at the end of the list becomes the
    // outermost layer, and each layer down moves one position to the left. Since we must evaluate the arguments
    // of a function before the function itself can be evaluated, this results in us evaluating the innermost f(a, b)
    // first, which will be the one from the furthest *left* of the list (the last we created, moving from right to left).
    // So when we unwind, we play the function chain from left to right, simulating foldLeft!
    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      ex3p7.foldRight(l, (b: B) => b)((a, g) => b => g(f(a, b)))(z)
    }
  }

  object ex3p14 extends Example {

    val name = "Ex3.14 - Implement append in terms of either foldLeft or foldRight"

    def append[A](as: List[A], append: List[A]): List[A] = {
      ex3p7.foldRight(as, append)( (a, acc) => Cons(a, acc))
    }
  }

}