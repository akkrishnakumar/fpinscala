package chapter3

import chapter3.List._
import scala.collection.mutable.ListBuffer

trait Chapter3

object Chapter3 {

  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil           => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }

  def setHead[A](as: List[A], head: A): List[A] =
    as match {
      case Nil           => sys.error("setHead on empty list")
      case Cons(_, tail) => Cons(head, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil           => Nil
      case cons: Cons[A] => if (n == 0) cons else drop(cons.tail, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
      case _                             => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(head, tail) if (tail != Nil) => Cons(head, init(tail))
      case _                                 => Nil
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0) { (_, count) =>
      count + 1
    }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft[Double, Double](l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, h) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A]) { (l, value) => Cons(value, l) }

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def appendUsingFoldRight[A](left: List[A], right: List[A]): List[A] =
    foldRight(left, right)(Cons(_, _))

  def appendUsingFoldLeft[A](l: List[A], r: List[A]): List[A] =
    reverse(foldLeft(r, reverse(l))((b, a) => Cons(a, b)))

  def concate[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendUsingFoldRight)
}
