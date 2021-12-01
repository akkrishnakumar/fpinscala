package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

}
