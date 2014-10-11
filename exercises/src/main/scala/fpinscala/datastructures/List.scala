package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case Nil => sys.error("tail of empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(oldHead, t) => Cons(h, t)
    case Nil => sys.error("setHead of empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n > 0) {
    l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  } else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case list => list
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(last, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => sys.error("init of empty list")
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((elem, len) => len + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def sumViaFoldLeft(nums: List[Int]): Int = foldLeft(nums, 0)(_ + _)

  def productViaFoldLeft(nums: List[Double]): Double = foldLeft(nums, 1.0)(_ * _)

  def lengthViaFoldLeft(l: List[_]): Int = foldLeft(l, 0)((len, _) => len + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((reversed, n) => Cons(n, reversed))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a, r) => Cons(a, r))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((tailList, a1Elem) => Cons(a1Elem, tailList))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())((outer, inner) => appendViaFoldLeft(outer, inner))

  def add1(nums: List[Int]): List[Int] = map(nums)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case Nil => Nil
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
    case Nil => Nil
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Cons(h, t) => appendViaFoldRight(f(h), flatMap(t)(f))
    case Nil => Nil
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) { h => if (f(h)) List(h) else Nil}

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Cons(a, t1), Cons(b, t2)) => Cons(a + b, addPairwise(t1, t2))
    case _ => Nil
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Cons(a, t1), Cons(b, t2)) => Cons(f(a, b), zipWith(t1, t2)(f))
    case _ => Nil
  }

  def startsWith[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Cons(h, t) if startsWith(Cons(h, t), sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
    case _ => false
  }
}
