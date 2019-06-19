package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    this match {
      case Empty       => Nil
      case Cons(x, xs) => x() :: xs().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(x, xs) if n > 0 => cons(x(), xs().take(n - 1))
      case _                    => empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, xs) if n > 0 => xs().drop(n - 1)
      case _                    => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
      case _                     => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b
    }

  def takeWhileFromFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else empty
    }

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Option(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty) { (a, b) =>
      cons(f(a), b)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) { (a, b) =>
      if (p(a)) cons(a, b) else b
    }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s) { (a, b) =>
      cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty) { (a, b) =>
      f(a).append(b)
    }

  def mapFromUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty       => None
      case Cons(x, xs) => Some((f(x()), xs()))
    }

  def takeFromUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(x, xs), n_) if n_ > 0 => Some((x(), (xs(), n_ - 1)))
      case _                           => None
    }

  def takeWhileFromUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(x, xs) if p(x()) => Some((x(), xs()))
      case _                     => None
    }

  def zipWithFromUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
      case _                          => None
    }

  def zipAllFromUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(a, as), Cons(b, bs)) =>
        Some((Some(a()), Some(b())), (as(), bs()))
      case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), empty))
      case (Empty, Cons(b, bs)) => Some((None, Some(b())), (empty, bs()))
      case _                    => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAllFromUnfold(s)
      .takeWhile {
        case (_, b) => b.isDefined
      }
      .forAll {
        case (a, b) => a == b
      }

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

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(current: Int, next: Int): Stream[Int] =
      cons(current, loop(next, current + next))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def fibsFromUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (current, next) => Some((current, (next, current + next)))
    }

  def fromFromUnfold(n: Int): Stream[Int] =
    unfold(n) { n =>
      Some(n, n + 1)
    }

  def constantFromUnfold[A](a: A): Stream[A] =
    unfold(a) { _ =>
      Some(a, a)
    }

  def onesFromUnfold: Stream[Int] =
    unfold(1) { _ =>
      Some(1, 1)
    }
}
