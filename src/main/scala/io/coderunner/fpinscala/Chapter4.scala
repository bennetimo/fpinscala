package io.coderunner.fpinscala

import scala.annotation.tailrec

trait Chapter4 {

  sealed trait Option[+A]{
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(a => f(a)) getOrElse None

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case _ => default
    }

    // Confusing one this one! We want to return the first option completely if it's a Some, otherwise
    // the second option. So first we map it so that if it was a Some(a), now it will be a Some(Some(a)),
    // otherwise it will still just be a None. Then we getOrElse it so in the first case we unwrap the first Some
    // to give us just Some(a), and in the second case its None so we return our default unmodified
    def orElse[B >: A](default: => Option[B]): Option[B] = map(a => Some(a)).getOrElse(default)

    def filter(f: A => Boolean): Option[A] = map(a => if(f(a)) Some(a) else None).getOrElse(None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object ex4p1 extends Example {

    val name = "Ex4.1 - Implement map, flatMap, getOrElse, orElse and filter for option"

  }

  object ex4p2 extends Example {

    val name = "Ex4.2 - Implement the variance function in terms of flatMap"

    def mean(xs: Seq[Double]): Option[Double] =
      if(xs.isEmpty) None
      else Some(xs.sum / xs.length)

    // The variance is the mean of math.pow(x - m, 2) for each element of the sequence where m is the mean
    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
    }
  }

  object ex4p3 extends Example {

    val name = "Ex4.3 - Write a generic function map2 that combines two Option values using a binary function"

    // Lift takes a function that works on standard A => B and 'lifts' it into the context of Option, so it now works
    // from Option[A] => Option[B]
    def lift[A, B](f: A => B): Option[A] => Option[B] = (a) => a map f

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      for {
        a <- a
        b <- b
      } yield f(a, b)
    }
  }

  object ex4p4 extends Example {

    val name = "Ex4.4 - Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original List"

    // If any option in the list is None, we return None. Otherwise we return a List of all the Some's.
    def sequence[A](opts: List[Option[A]]): Option[List[A]] = {
      @tailrec
      def loop(o: List[Option[A]], res: List[A]): Option[List[A]] = o match {
        case Nil => Some(res.reverse)
        case Some(x) :: t => loop(t, x :: res)
        case _ => None
      }
      loop(opts, Nil)
    }
    
  }

}