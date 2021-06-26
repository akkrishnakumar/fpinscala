package chapter3

object Ex3_3 {

  def setHead[A](as: List[A], head: A): List[A] =
    as match {
      case Nil           => sys.error("setHead on empty list")
      case Cons(_, tail) => Cons(head, tail)
    }

}
