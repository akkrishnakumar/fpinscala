package chapter3

import Ex3_10.foldLeft

object Ex3_12 {

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A]) { (l, value) => Cons(value, l) }

}
