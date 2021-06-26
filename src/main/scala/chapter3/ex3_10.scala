package chapter3

object Ex3_10 {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

}
