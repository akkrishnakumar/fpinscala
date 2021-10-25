import java.util.concurrent.TimeUnit
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.Callable

package object chapter7 {

  //trait Par[+A]
  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true

    override def isCancelled: Boolean = false

  }

  implicit class ParExt[A](self: Par[A]) {

    def get(a: Par[A]): A = ???

    def map2[B, C](that: Par[B])(f: (A, B) => C): Par[C] =
      es => Par.MapFuture2(self(es), that(es), f)

    def map[B](f: A => B): Par[B] =
      self.map2(Par.unit(()))((a, _) => f(a))

    def run(es: ExecutorService): Future[A] = self(es)
  }

  object Par {

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](a: => Par[A]): Par[A] =
      es =>
        es.submit(new Callable[A] {
          override def call(): A = a(es).get
        })

    def MapFuture2[A, B, C](left: Future[A], right: Future[B], f: (A, B) => C): Future[C] =
      UnitFuture(f(left.get(), right.get()))

    def sequence[A](l: List[Par[A]]): Par[List[A]] =
      l.foldRight(unit(Nil: List[A])) { (a, acc) => a.map2(acc)(_ :: _) }

  }
}
