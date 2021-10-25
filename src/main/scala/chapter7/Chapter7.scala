package chapter7

import Par._

object Chapter7 {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l).map2(sum(r))(_ + _)
    }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val parL = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    sequence(parL).map(_.flatten)
  }

}
