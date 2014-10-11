package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  @tailrec
  final def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists2(p)
    case Empty => false
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  @tailrec
  final def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).dropWhile(opts => opts._1 == opts._2).take(1).forAll {
    case (a, None) => true
    case _ => false
  }

  def toList: List[A] = {
    val buffer = collection.mutable.ListBuffer[A]()
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buffer += h()
        go(t())
      case _ => buffer.toList
    }
    go(this)
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this, f) {
    case (Cons(h, t), p) if p(h()) => Some(h(), (t(), f))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, s2) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold(this, s2) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (Empty, t2()))
    case (Empty, Empty) => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s2)((_, _))

  def tails: Stream[Stream[A]] = unfold[Stream[A], Stream[A]](this) {
    case Cons(h, t) =>
      lazy val tail = t()
      Some(Cons(h, t), tail)
    case Empty => None
  }.append(Stream(empty))

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] =
    foldRight((Stream(s), s)) { (elem, resultAndState) =>
      val next = f(elem, resultAndState._2)
      (Stream.cons(next, resultAndState._1), next)
    }._1

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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constantStacking[A](a: A): Stream[A] = Cons(() => a, () => constantStacking(a))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail // gibt immer dasselbe Objekt zurück, statt neue zu generieren wie cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  lazy val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((x, next)) => Stream.cons(x, unfold(next)(f))
    case None => Stream.empty
  }

  lazy val fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b))}

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  lazy val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}