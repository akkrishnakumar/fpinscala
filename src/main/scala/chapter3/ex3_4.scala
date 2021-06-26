package chapter3

import scala.collection.immutable

object Ex3_4 {

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil           => Nil
      case cons: Cons[A] => if (n == 0) cons else drop(cons.tail, n - 1)
    }

}
