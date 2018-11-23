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

  object ex4p5 extends Example {

    val name = "Ex4.5 - Write the function traverse to map over a list using a function f that might fail, returning None if applying f ever returns None"

    // This is basically the equivalent of first mapping each element of the list to get a List[Option[B]], and then sequencing it.
    // But we want something more efficient, that doesn't traverse the list completely twice.
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => {
        //Apply the function to the head which gives us back an option. Then concatenate the head value
        //with the result traversing the result of the rest of the list, flatMapping it to give us just one layer of Option
        f(h).flatMap(b => ex4p3.map2(Some(b), traverse(t)(f))(_ :: _))

        //The solutions use a slightly simpler:
        // ex4p3.map2(f(h), traverse(t)(f))(_ :: _)
      }
    }

    def sequenceViaTraverse[A](opts: List[Option[A]]): Option[List[A]] = {
      traverse(opts)(a => a)
    }
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    // When mapping over the right side, we must promote the left type paramater to some supertype, to satisfy the +E variance annotation
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(_) => this
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        rightA <- this
        rightB <- b
      } yield f(rightA, rightB)
    }
  }
  case class Left[E](value: E) extends Either[E, Nothing]
  case class Right[A](value: A) extends Either[Nothing, A]

  object ex4p6 extends Example {

    val name = "Ex4.6 - Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value"
  }

  object ex4p7 extends Example {

    val name = "Ex4.7 - Implement sequence and traverse for Either. They should return the first error encountered if there is one"

    // Traverse passes through the list applying a function to each item, returning the whole list if there is no failures
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h :: t => {
        val head = f(h).map(b => List(b))
        val tail = traverse(t)(f)

        (head map2 tail)(_ ++ _)
      }
    }

    // Sequence is similar to traverse, but it doesn't care about applying the function. It just wants to check the list as it
    // stands already for any failures
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)
  }

  object ex4p8 extends Example {

    val name = "Ex4.8 - What would you change to be able to collect multiple errors, not just one?"

    // You'd need to change the type of the left (error) from E to a List[E].
    // Wouldn't change mkPerson because it shouldn't have to care about how errors are handled, just does its localised piece
    // Instead, we would create a new data type that accumulates errors on the left
    // flatMap won't work to accumulate failures with a Monad. Why? Because the function is only executed in the success case,
    // so there is no way to proceed as soon as a single failure occurs
  }

}