package io.coderunner.fpinscala

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait Chapter2 {

  object ex2p1 extends Example {

    val name = "Ex2.1 Write a tail recursive function to get the nth fibonacci number"

    def fib(n: Int): Int = {
      @tailrec
      def go(n: Int, prev: Int, prevprev: Int): Int = n match {
        case 1 => prevprev
        case 2 => prev
        case x if n > 0 => go(n-1, prev+prevprev, prev)
        case _ => 0 //Not defined for negative n
      }
      go(n, 1, 0)
    }

    def fibAll(n: Int): List[Int] = {
      @tailrec
      def go(n: Int, terms: List[Int]): List[Int] = {
        terms match {
          case Nil if n > 0 => go(n-1, 0 :: terms)
          case 0 :: Nil if n > 0  => go(n-1, 1 :: terms)
          case p1 :: p2 :: _ if n > 0  => go(n-1, p1 + p2 :: terms)
          case _ => terms //Not defined for negative n
        }
      }
      go(n, Nil).reverse
    }

    def fibNotTR(n: Int): Int = {
      if(n <= 1) 0
      else if (n == 2) 1
      else fibNotTR(n-1) + fibNotTR(n-2)
    }

    def fib2(n: Int): Int = {
      @tailrec
      def loop(n: Int, prev: Int, next: Int): Int = n match {
        case x if x <= 0 => 0
        case 1 => prev
        case 2 => next
        case x => loop(n-1, next, prev+next)
      }
      loop(n, 0, 1)
    }

  }

  object ex2p2 extends Example {

    val name = "Ex2.2 Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function"

    @tailrec
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      as.length match {
        case 0 => true
        case 1 => true
        case 2 => ordered(as(0), as(1))
        case _ => ordered(as(0), as(1)) && isSorted(as.tail, ordered)
      }
    }

    def isSorted2[A: ClassTag](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
      as.zip(if (as.isEmpty) Array.empty[A] else as.tail).forall(a => ordered(a._1, a._2))
    }

    def isSorted3[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @tailrec
      def loop(n: Int): Boolean = {
        if(n >= as.length - 1) true //We've got all the way to the end of the array without a problem, so it must be sorted
        else if(!ordered(as(n), as(n+1))) false
        else loop(n+1)
      }
      loop(0)
    }

  }

}