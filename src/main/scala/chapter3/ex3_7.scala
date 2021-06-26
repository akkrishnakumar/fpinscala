package chapter3

object Ex3_7 {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)(_ + _)

}
