package chapter3

import Ex3_7.foldRight

object Ex3_9 {

  def length[A](as: List[A]): Int =
    foldRight(as, 0) { (_, count) =>
      count + 1
    }

}
