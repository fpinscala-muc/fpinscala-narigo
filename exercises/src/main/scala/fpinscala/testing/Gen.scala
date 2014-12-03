package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._
import fpinscala.state.RNG
import fpinscala.state.State
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 {
  self =>
  def check: Boolean

  def &&(p: Prop0): Prop0 = new Prop0 {
    def check: Boolean = self.check && p.check
  }
}

trait Prop1 {
  self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop1): Prop1 =
    new Prop1 {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (tc, rng) =>
    run(tc, rng) match {
      case Passed | Proved => p.run(tc, rng)
      case fResult => fResult
    }
  }

  def ||(p: Prop): Prop = Prop { (tc, rng) =>
    run(tc, rng) match {
      case Falsified(msg, s) => p.tag(msg).run(tc, rng)
      case o => o
    }
  }

  def tag(msg: String) = Prop { (tc, rng) =>
    run(tc, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case $s
        |generated an exception: ${e.getMessage}
        |stack trace:
        |${e.getStackTrace.mkString("\n")}""".stripMargin
}

object ListProps {
  // Exercise 8.14: Prop for List.sorted
  lazy val intListGen: Gen[List[Int]] = Gen.choose(0, 20).flatMap(n => Gen.listOfN(n, Gen.choose(-10, 10)))

  lazy val sortedProp: Prop =
    Prop.forAll(intListGen) { ns: List[Int] =>
      val sorted = ns.sorted

      // alle elemente sind auch im sorted drin
      ns.forall(n => sorted.count(_ == n) == ns.count(_ == n)) &&
        // jedes nachfolgende element ist >= dem angeschauten
        sorted.zipWithIndex.forall { case (a, idx) =>
          sorted.drop(idx).forall(_ >= a)
        }
    }

  // Exercise 8.14: Prop for List.takeWhile
  lazy val takeWhileProp: Prop = {
    val f = (_: Int) <= 0
    val p1 = Prop.forAll(intListGen) { l: List[Int] =>
      l.takeWhile(f).forall(f)
    }
    val p2: Prop = Prop.forAll(intListGen) { l: List[Int] =>
      l == l.takeWhile(f) ++ l.dropWhile(f)
    }
    p1 && p2
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

  // here is an example on how to combine generators in a for-comprehension
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    for {
      b <- Gen.boolean
      a <- gen
    } yield if (b) Some(a) else None

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def stringN(n: Int): Gen[String] =
    listOfN(n, union(choose('a'.toInt, 'z'.toInt), choose('A'.toInt, 'Z'.toInt))).map(_.map(_.toChar).mkString)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(RNG.double).flatMap { d =>
    val g1Abs = Math.abs(g1._2)
    val g2Abs = Math.abs(g2._2)
    val threshold = g1Abs / (g1Abs + g2Abs)
    if (d < threshold) g1._1.sample else g2._1.sample
  })

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => g.listOfN(i))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i => g.listOfN(Math.max(i, 1)))

  lazy val parInt: Gen[Par[Int]] = //choose(-100, 100).map(Par.unit)
    choose(-100, 100).listOfN(choose(0, 20)).map { l =>
      l.foldLeft(Par.unit(0)) { (p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        }
      }
    }

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen {
    sample.map(f)
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen.listOfN(_, this))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(apply(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i => forSize(i).flatMap(a => f(a).forSize(i))}

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen { i =>
    apply(i) ** s2.apply(i)
  }
}
