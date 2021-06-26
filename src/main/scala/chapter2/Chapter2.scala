package chapter2

import scala.annotation.tailrec

trait Chapter2

object Chapter2 {

  def fib(n: Int): Int = {
    if (n < 1) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    go(0)
  }

  // def isSorted[A](n: Array[A], ordered: (A, A) => Boolean): Boolean =
  //   (0 until n.size - 1)
  //     .map(i => ordered(n(i), n(i + 1)))
  //     .forall(identity)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
