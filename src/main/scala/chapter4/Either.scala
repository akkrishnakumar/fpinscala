package chapter4

import scala.collection.immutable

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e)      => Left(e)
      case Right(value) => Right(f(value))
    }

  def flatMap[B, EE >: E](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)      => Left(e)
      case Right(value) => f(value)
    }

  def orElse[AA >: A, EE >: E](b: Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(e)      => b
      case Right(value) => Right(value)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}

object Either {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil    => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil    => Right(Nil)
      case h :: t => h.map2(sequence(t))(_ :: _)
    }

}

case class Left[+E](e: E)      extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
