package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(x)             => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(x)             => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(x)             => f(x)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeFromFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximumFromFold(tree: Tree[Int]): Int =
    fold(tree)(x => x) { (left, right) =>
      left max right
    }

  def depthFromFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0) { (left, right) =>
      1 + (left max right)
    }

  def mapFromFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B]) { (left, right) =>
      Branch(left, right)
    }
}
