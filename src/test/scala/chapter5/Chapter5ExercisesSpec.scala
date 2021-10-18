package chapter5

import utils.BaseSpec
import Chapter5._

class Chapter5ExercisesSpec extends BaseSpec {

  test("Ex 5.1 - Convert a Stream to a List") {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  test("Ex 5.2.1 - Implement take(n) method for Stream") {
    Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
  }

  test("Ex 5.2.2 - Implement drop(n) method for Stream") {
    Stream(1, 2, 3, 4, 5).drop(2).toList shouldBe List(3, 4, 5)
  }

  test("Ex 5.3 - Implement takeWhile method for Stream") {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList shouldBe List(1, 2)
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 0).toList shouldBe Nil
  }

  test("Ex 5.4 - Implement forAll method for Stream") {
    Stream(1, 2, 3).forAll((x: Int) => x % 2 == 0) shouldBe false
    Stream("a", "b", "c").forAll((x: String) => x.size > 0) shouldBe true
  }

  ignore("Ex 5.5 - Implement takeWhile method using foldRight") {
    // Implemented method - takeWhileViaFoldRight
  }

  ignore("Ex 5.6 - Implement headOption method using foldRight") {
    // Implemented method - headOption
  }

  ignore("Ex 5.7.1 - Implement map method using foldRight") {
    // Implemented method - mapViaFoldRight
  }

  ignore("Ex 5.7.2 - Implement filter method using foldRight") {
    // Implement method - filterViaFoldRight
  }

  ignore("Ex 5.7.3 - Implement append method using foldRight") {
    // Implement method - appendViaFoldRight
  }

  ignore("Ex 5.7.4 - Implement flatMap method using foldRight") {
    // Implement method - flatMapViaFoldRight
  }

  test("Ex 5.4.x - Infinite stream example") {
    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
    ones.exists(_ % 2 != 0) shouldBe true
    ones.map(_ + 1).exists(_ % 2 == 0) shouldBe true
    ones.forAll(_ != 1) shouldBe false
  }

  ignore("Ex 5.8 - Implement constant method which returns an infinite stream of a given value") {
    // Implmeneted method - constant
  }

  test(
    "Ex 5.9 - Implement from method which returns an infinite stream of integers, adding 1 on every iteration "
  ) {
    from(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  test("Ex 5.10 - Implement fibs method which generates an infinite stream of fibonacci numbers") {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  test(
    "Ex 5.11 - Implement unfold method which creates an infinite with initial state and function which generates next state and next value"
  ) {
    // Implement method - unfold
  }

  test("Ex 5.12.1 - Implement fibs method using unfold") {
    fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  test("Ex 5.12.2 - Implement from method using unfold") {
    fromViaUnfold(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  test("Ex 5.12.3 - Implement constant method using unfold") {
    // Implemented constant
  }

  test("Ex 5.12.4 - Implement ones method using unfold") {
    onesViaUnfold.take(10).toList shouldBe List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  }

  test("Ex 5.13.1 - Implement map method using unfold") {
    // Implemented method - mapViaUnfold
  }

  test("Ex 5.13.2 - Implement take method using unfold") {
    Stream(1, 2, 3, 4, 5).takeViaUnfold(4).toList shouldBe List(1, 2, 3, 4)
  }

  test("Ex 5.13.3 - Implement takeWhile method using unfold") {
    // Implemented method - takeWhileViaUnfold
  }

  test("Ex 5.13.4 - Implement zipWith method using unfold") {
    // Implemented method - zipWithViaUnfold
  }

  test("Ex 5.13.5 - Implement zipAll method using unfold") {
    // Implemented method - zipAllViaUnfold
  }

  test("Ex 5.14 - Implement startsWith method for Stream") {
    Stream(1, 2, 3) startsWith Stream(1, 2) shouldBe true
  }

  test("Ex 5.15 - Implement tails method for Stream") {
    Stream(1, 2, 3).tails.toList
      .map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  test("Ex 5.16 - Implement scanRight method for Stream") {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }

}
