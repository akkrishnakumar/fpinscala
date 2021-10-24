package chapter5

trait Chapter5

object Chapter5 {

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Stream.cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Stream.cons(n, from(n + 1))
    tail
  }

  def fibs: Stream[Int] = {
    def fibo(n1: Int, n2: Int): Stream[Int] =
      Stream.cons(n1, fibo(n2, n1 + n2))
    fibo(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some(v) => Stream.cons(v._1, unfold(v._2)(f))
      case None    => Stream.empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (n1, n2) => Some((n1, (n2, n1 + n2))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(100)(a => Some((a, a + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold =
    unfold(1)(_ => Some(1, 1))

}
