package chapter3

object Ex3_6 {

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(head, tail) if (tail != Nil) => Cons(head, init(tail))
      case _                                 => Nil
    }

}
