package chapter3

sealed trait List[+A]
case object Nil                                     extends List[Nothing]
case class Cons[+A](val head: A, val tail: List[A]) extends List[A]

object List {

  def sum(as: List[Int]): Int =
    as match {
      case Nil              => 0
      case Cons(head, tail) => head + sum(tail)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
