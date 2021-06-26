package chapter3

import Ex3_10.foldLeft

object Ex3_11 {
  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft[Double, Double](l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, h) => acc + 1)
}
