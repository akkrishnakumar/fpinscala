package chapter4

trait Chapter4

object Chapter4 {

  def getDepartment: (Option[Employee]) => Option[String] =
    _.map(_.department)

  def getManager: (Option[Employee]) => Option[String] =
    _.flatMap(_.manager)

  private def mean(xs: Seq[Double]): Option[Double] =
    Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft[Option[List[A]]](Some(Nil))((b, a1) => b.flatMap(bb => a1.map(bb :+ _)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft[Option[List[B]]](Some(Nil))((b, a1) => map2(b, f(a1))(_ :+ _))

  def getDepartment(e: Either[String, Employee]): Either[String, String] = e.map(_.department)
}
