package chapter3

object Ex3_5 {

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
      case _                             => l
    }

}
