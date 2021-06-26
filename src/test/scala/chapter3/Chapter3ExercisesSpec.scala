package chapter3

import utils.BaseSpec
import chapter3.Chapter3._
import chapter3.List._

class Chapter3ExercisesSpec extends BaseSpec {

  test("Ex 3.1 - x == 3 (x is a computation of match experssion)") {
    x shouldEqual 3
  }

  test("Ex 3.2 - def tail[A](as: List[A]): List[A]") {
    tail(List(1, 2, 3)) shouldEqual List(2, 3)
    tail(List(1)) shouldEqual Nil
  }

  test("Ex 3.3 - def setHead[A](as: List[A], head: A): List[A]") {
    setHead(List(1, 2, 3), 3) shouldEqual List(3, 2, 3)
    setHead(List("a", "b"), "c") shouldEqual List("c", "b")
  }

  test("Ex 3.4 - def drop[A](l: List[A], n: Int): List[A]") {
    drop(List(1, 2, 3), 1) shouldEqual List(2, 3)
    drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
    drop(List("a", "b"), 2) shouldEqual Nil
    drop(List(1, 2), 3) shouldEqual Nil
    drop(Nil, 1) shouldEqual Nil
  }

  test("Ex 3.5 - def dropWhile[A](l: List[A], f: A => Boolean): List[A]") {
    dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldEqual List(2, 3)
    dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldEqual List(1, 2, 3)
    dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldEqual Nil
    dropWhile(Nil, (x: Int) => x > 0) shouldEqual Nil
  }

  test("Ex 3.6 - def init[A](l: List[A]): List[A]") {
    init(List(1, 2, 3)) shouldEqual List(1, 2)
    init(List(1)) shouldEqual Nil
  }

  ignore("Ex 3.7 - short circuiting List.product() using foldRight") {
    // I think it is not possible
  }

  test("Ex 3.8 - foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))") {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldEqual Cons(1, Cons(2, Cons(3, Nil)))
  }

  test("Ex 3.9 - def length[A](as: List[A]): Int") {
    Chapter3.length(List(1, 2, 3, 4, 5)) shouldEqual 5
  }

  ignore("Ex 3.10 - def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B") {
    // Just implemented the foldLeft function
  }

  test("Ex 3.11 - sum, product using foldLeft") {
    sum3(List(1, 2, 3, 4, 5)) shouldEqual 15
    product3(List(1.0, 2.0, 3.0)) shouldEqual 6.0
    length2(List(1, 2, 3, 4, 5)) shouldEqual 5
  }

  test("Ex 3.12 - def reverse[A](as: List[A]): List[A]") {
    reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
  }

  ignore("Ex 3.13 - foldLeft in terms of foldRight") {
    // Just implemented foldLeftViaFoldRight
  }

  ignore("Ex 3.13 - foldRight in terms of foldLeft") {
    // Just implemented foldRightViaFoldLeft
  }

  test("Ex 3.14 - append using foldRight") {
    appendUsingFoldRight(List(1, 2, 3), List(1, 2)) shouldEqual List(1, 2, 3, 1, 2)
    appendUsingFoldRight(List(1, 2, 3), Nil) shouldEqual List(1, 2, 3)
    appendUsingFoldRight(Nil, List(1, 2)) shouldEqual List(1, 2)
    appendUsingFoldRight(Nil, Nil) shouldEqual List()
  }

  test("Ex 3.14 - append using foldLeft") {
    appendUsingFoldLeft(List(1, 2, 3), List(1, 2)) shouldEqual List(1, 2, 3, 1, 2)
    appendUsingFoldLeft(List(1, 2, 3), Nil) shouldEqual List(1, 2, 3)
    appendUsingFoldLeft(Nil, List(1, 2)) shouldEqual List(1, 2)
    appendUsingFoldLeft(Nil, Nil) shouldEqual List()
  }

  test("Ex 3.15 - concate List[List[A]] into a List[A]. (Linear time)") {
    concate(List(List(1, 2), List(3, 4))) shouldEqual List(1, 2, 3, 4)
  }

}