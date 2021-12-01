package chapter10

import scala.annotation.tailrec
import chapter3.Tree
import chapter3.Leaf
import chapter3.Branch

object Chapter10 {

  val intAddition: Monoid[Int] =
    new Monoid[Int] {
      override def op(a: Int, b: Int): Int = a + b
      override def identity: Int           = 0
    }

  val intMultiplication: Monoid[Int] =
    new Monoid[Int] {
      override def op(a: Int, b: Int): Int = a * b
      override def identity: Int           = 1
    }

  val booleanOr: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def op(a: Boolean, b: Boolean): Boolean = a || b
      override def identity: Boolean                   = false
    }

  val booleanAnd: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def op(a: Boolean, b: Boolean): Boolean = a && b
      override def identity: Boolean                   = true
    }

  implicit def add2String(a: String, b: String): String = a + b

  def optionMonoid[A](implicit f: (A, A) => A): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def op(a: Option[A], b: Option[A]): Option[A] =
        a.foldRight(b)((x, y) => y.map(f(x, _)).orElse(Some(x)))

      override def identity: Option[A] = None
    }

  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      override def op(a: A => A, b: A => A): A => A = a andThen b
      override def identity: A => A                 = (a: A) => a
    }

  def stringSeparatedByCommaMonoid: Monoid[String] =
    new Monoid[String] {
      override def op(a: String, b: String): String = s"${a.trim}, ${b.trim}"
      override def identity: String                 = ""
    }

  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B =
    l.foldRight(m.identity)((a, b) => m.op(f(a), b))

  def foldMapV[A, B](s: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    s.length match {
      case 1 => f(s.head)
      case _ =>
        val (l, r) = s.splitAt(s.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def wcMonoid: Monoid[WC] =
    new Monoid[WC] {

      override def op(a: WC, b: WC): WC =
        (a, b) match {
          case (Part(l1, cnt1, r1), Part(l2, cnt2, r2)) => Part(l1, cnt1 + cnt2 + 1, r2)
          case (Part(l, cnt, r), Stub(chars))           => Part(l, cnt, r + chars)
          case (Stub(chars), Part(l, cnt, r))           => Part(chars + l, cnt, r)
          case (Stub(chars1), Stub(chars2))             => Stub(chars1 + chars2)
        }

      override def identity: WC = Stub("")

    }

  implicit class StringExt(self: String) {

    def toWCs: List[WC] =
      if (self.size <= 10) {
        List(self.toWC)
      } else {
        val (l, r) = self.splitAt(self.size / 2)
        l.toWCs ::: r.toWCs
      }

    def toWC: WC = {
      val segments = self.split(" ")
      segments.size match {
        case 1 => Stub(segments(0))
        case 2 => Part(segments(0), 1, "")
        case _ => Part(segments(0), segments.drop(1).dropRight(1).size, segments(segments.size - 1))
      }
    }

  }

  def wordCount(s: String): Int =
    WC.count(s.toWCs.fold(wcMonoid.identity)((a, b) => wcMonoid.op(a, b)))

  def foldableList: Foldable[List] =
    new Foldable[List] {

      override def foldRight[A, B](as: List[A])(b: B)(f: (A, B) => B): B =
        as.foldRight(b)(f)

      override def foldLeft[A, B](as: List[A])(b: B)(f: (B, A) => B): B =
        as.foldLeft(b)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
        foldRight(as)(m.identity)((a, b) => m.op(f(a), b))

    }

  def foldableTree: Foldable[Tree] =
    new Foldable[Tree] {

      override def foldRight[A, B](as: Tree[A])(b: B)(f: (A, B) => B): B =
        as match {
          case Leaf(a)             => f(a, b)
          case Branch(left, right) => foldRight(left)(foldRight(right)(b)(f))(f)
        }

      override def foldLeft[A, B](as: Tree[A])(b: B)(f: (B, A) => B): B =
        as match {
          case Leaf(a)             => f(b, a)
          case Branch(left, right) => foldLeft(right)(foldLeft(left)(b)(f))(f)
        }

      override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
        foldRight(as)(m.identity)((a, b) => m.op(f(a), b))

    }

  def foldableOption: Foldable[Option] =
    new Foldable[Option] {

      override def foldRight[A, B](as: Option[A])(b: B)(f: (A, B) => B): B =
        as.map(a => f(a, b)).getOrElse(b)

      override def foldLeft[A, B](as: Option[A])(b: B)(f: (B, A) => B): B =
        as map (f(b, _)) getOrElse b

      override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
        foldRight(as)(m.identity)((a, b) => m.op(f(a), b))

    }

  def productMonoid[A, B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a: (A, B), b: (A, B)): (A, B) = (m1.op(a._1, b._1), m2.op(a._2, b._2))
      override def identity: (A, B)                 = (m1.identity, m2.identity)
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(f1: A => B, f2: A => B): A => B = a => b.op(f1(a), f2(a))
      override def identity: A => B                   = _ => b.identity
    }

  def occuranceMap[A]: Monoid[Map[A, Int]] =
    new Monoid[Map[A, Int]] {

      override def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
        (a.keySet ++ b.keySet)
          .map(k => (k, a.getOrElse(k, 0) + b.getOrElse(k, 0)))
          .toMap

      override def identity: Map[A, Int] = Map.empty

    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as
      .map(a => Map(a -> 1))
      .fold(occuranceMap.identity)(occuranceMap.op)

}
