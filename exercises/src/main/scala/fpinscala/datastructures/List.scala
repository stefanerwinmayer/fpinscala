package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil         => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil         => List(h)
      case Cons(_, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (x <= 0) {
      l
    } else {
      drop(List.tail(l), n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _                   => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(x, xs)        => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (_, b) =>
      b + 1
    }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0) { (b, _) =>
      b + 1
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (b, a) =>
      Cons(a, b)
    }

  def foldLeftFromRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z) { (a, b) =>
      f(b, a)
    }

  def foldRightFromLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (b, a) =>
      f(a, b)
    }

  def appendFromFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) { Cons(_, _) }

  def flatConcat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def mapPlusOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]()) { (n, l) =>
      Cons(n + 1, l)
    }

  def doubleListToStringList(l: List[Double]): List[String] =
    foldRight(l, List[String]()) { (n, l) =>
      Cons(n.toString, l)
    }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]()) { (a, b) =>
      Cons(f(a), b)
    }

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, List[A]()) { (a, b) =>
      if (p(a)) Cons(a, b) else b
    }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    flatConcat(map(l)(f))

  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l) { a =>
      if (p(a)) List(a) else List()
    }

  def zip(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zip(xs, ys))
    }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1, a2) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def loop(sup: List[A], sub: List[A]): Boolean =
      (sup, sub) match {
        case (_, Nil)                             => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => loop(xs, ys)
        case _                                    => false
      }

    sup match {
      case Nil         => false
      case Cons(_, xs) => loop(sup, sub) || hasSubsequence(xs, sub)
    }
  }
}
