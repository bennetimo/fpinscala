package io.coderunner.fpinscala

import scala.annotation.tailrec

trait Chapter6 {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    def nextInt: (Int, RNG)
  }

  // A Linear congruential generator
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object ex6p1 extends Example {

    val name = "Ex6.1 - Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue"

    def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      case (r, rng) if r < 0 => Math.abs(r + 1) -> rng
      case (r, rng) => Math.abs(r) -> rng
    }
  }

  object ex6p2 extends Example {

    val name = "Ex6.2 - Write a function to generate a Double between 0 and 1, not including 1"

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = ex6p1.nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }
  }

  object ex6p3 extends Example {

    import ex6p1._
    import ex6p2._

    val name = "Ex6.3 - Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple"

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = rng.nextInt
      val (i2, r2) = double(r)
      ((i, i2), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, r) = double(rng)
      val (i2, r2) = nonNegativeInt(r)
      ((d, i2), r2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r) = double(rng)
      val (d2, r2) = double(r)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }
  }

  object ex6p4 extends Example {

    import ex6p1._
    import ex6p2._

    val name = "Ex6.4 - Write a function to generate a list of random integers"

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def loop(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
        val (i, r) = rng.nextInt
        if (n <= 0) acc -> rng
        else loop(n-1, i :: acc, r)
      }
      loop(count, Nil, rng)
    }
  }

  object ex6p5 extends Example {

    import ex6p1._

    val name = "Ex6.5 - Use map to implement double in a more elegant way"

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  object ex6p6 extends Example {

    val name = "Ex6.6 - Write the implementation of map2 that takes two actions ra and rb, and a function f for combining their results"

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

    import ex6p2._

    val int: Rand[Int] = _.nextInt
    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)
  }

  object ex6p7 extends Example {

    val name = "Ex6.7 (Hard) - Implement sequence for combining a List of transitions into a single transition"

    import ex6p5._
    import ex6p6._

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))
  }

  object ex6p8 extends Example {

    val name = "Ex6.8 - Implement flatMap, and then use it to implement nonNegativeLessThan"

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

    import ex6p1._
    import ex6p5._
    def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod )else nonNegativeLessThan(n)
    })
  }

  object ex6p9 extends Example {

    val name = "Ex6.9 - Reimplement map and map2 in terms of flatMap"

    import ex6p5._
    import ex6p8._
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)( a => map(rb)(b => f(a, b)))
  }

  import State._
  case class State[S, +A](run: S => (A, S)){

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap( a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    type Randy[A] = State[RNG, A]

    def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List.empty))( (f, acc) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  object ex6p10 extends Example {

    val name = "Ex6.10 - Generalise the functions unit, map, map2, flatMap and sequence"

    import ex6p5._
    import ex6p8._
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)( a => map(rb)(b => f(a, b)))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  // Code below taken from solutions
  object Machine {
    //Rules:
    //1. Inserting a coin into a locked machine will cause it to unlock if there's any candy left
    //2. Turning the knob on an unlocked machine will cause it to dispense candy and become locked
    //3. Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
    //4. A machine that's out of candy ignores all inputs

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequenceViaFoldRight(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

    def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s //Anything on a machine with no candies does nothing
        case (Coin, Machine(false, _, _)) => s //Inserting a coin into an unlocked machine does nothing
        case (Turn, Machine(true, _, _)) => s //Turning the knob on a locked machine does nothing
        case (Coin, Machine(true, candy, coin)) => //Inserting a coin into a locked machine unlocks it and increases the coin count
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) => //Turn the coin of an unlocked machine gives out a candy
          Machine(true, candy - 1, coin)
      }
  }

  object ex6p11 extends Example {

    val name = "Ex6.11 (Hard) - Implement a series of finite state automaton that models a simple candy dispenser"
  }

}