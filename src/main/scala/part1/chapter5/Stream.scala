package chapter5

sealed trait Stream[+A] {

  import Stream._

  def map[B](f: A => B): Stream[B] =
    this match {
      case Cons(h, t) => cons(f(h()), t().map(f))
      case Empty      => empty
    }

  def exists(f: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => f(h()) || t().exists(f)
      case _          => false
    }

  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  override def toString(): String =
    this match {
      case Cons(h, t) => s"Cons(${h()}, ${t().toString()})"
      case Empty      => s"Empty"
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _                   => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (f(h())) => cons(h(), t().takeWhile(f))
      case _                      => empty
    }

  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(b)(f))
      case _          => b
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (f(a)) cons(a, b) else empty
    }

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filterViaFoldRight[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def appendViaFoldRight[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) appendViaFoldRight b)

  private def unfold[A, B](b: B)(f: B => Option[(A, B)]): Stream[A] =
    f(b) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None         => empty
    }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if (n > 0) => Some((h(), (t(), n - 1)))
      case _                          => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _                      => None
    }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty)               => None
      case (Cons(h, t), Empty)          => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t))          => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAllViaUnfold(s)
      .takeWhile(!_._2.isEmpty)
      .forAll {
        case (h1, h2) => h1 == h2
      }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case Empty      => None
    } appendViaFoldRight (cons(empty, empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2      = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}

case object Empty                                         extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
