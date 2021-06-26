package chapter2

import scala.annotation.tailrec

object Ex2_2 {

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

}
