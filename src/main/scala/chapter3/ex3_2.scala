package chapter3

object Ex3_2 {

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil           => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }

}
