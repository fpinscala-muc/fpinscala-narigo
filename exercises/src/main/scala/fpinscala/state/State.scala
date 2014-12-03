package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (-(i + 1), rng2)
    else (i, rng2)
  }

  def boolean(rng: RNG) = nonNegativeInt(rng) match {
    case (i, rng2) => (i % 2 == 0, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r2) = double(rng)
    val (i, r3) = r2.nextInt
    ((d, i), r3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, is: List[Int], r: RNG): (List[Int], RNG) = {
      if (c == 0) {
        (is, r)
      } else {
        val (x, r2) = r.nextInt
        go(c - 1, x :: is, r2)
      }
    }
    go(count, Nil, rng)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng1 =>
    val (randA, rng2) = ra(rng1)
    val (randB, rng3) = rb(rng2)
    (f(randA, randB), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))((ra, rList) => map2(ra, rList)(_ :: _))

//  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
//    val (a: List[A], rng2: RNG) = ???
//    (a, rng2)
//  }

  // Wenn Rand[A] ein State[RNG, A] wäre und wir map/flatMap in dem Typ hätten, ginge das:
  //  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] = {
  //    fs.foldRight(unit(List[A]())) {
  //      (ra, rla) => for {
  //        a: A <- ra
  //        la: List[A] <- rla
  //      } yield (a :: la)
  //    }
  //  }

  //    fs.foldRight[Rand[List[A]]](unit(Nil))((ra, rList) => map2(ra, rList)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng0 =>
    val (a, rng1) = f(rng0)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      mapViaFlatMap(rb)(b => f(a, b))
    }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s0 =>
    val (a, s1) = run(s0)
    f(a).run(s1)
  })

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight[State[S, List[A]]](unit(Nil))((sa, sList) => sa.map2(sList)(_ :: _))

  // imports get and set
  import fpinscala.applicative.StateUtil._

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence {
      val l = inputs.map(input => modify[Machine](machine => (input, machine) match {
        case (_, m@Machine(_, 0, _)) => m
        case (Turn, m@Machine(true, _, _)) => m
        case (Coin, m@Machine(false, _, _)) => m
        case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
      }))
      l
    }
    machine <- get
  } yield (machine.coins, machine.candies)

}