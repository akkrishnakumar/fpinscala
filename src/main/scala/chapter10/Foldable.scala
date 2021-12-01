package chapter10

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(b: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(b: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.identity)(m.op)
}
