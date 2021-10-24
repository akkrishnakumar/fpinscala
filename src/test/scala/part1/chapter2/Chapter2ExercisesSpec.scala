import chapter2.Chapter2._
import utils.BaseSpec

class Chapter2ExercisesSpec extends BaseSpec {

  test("5th Fibonacci should be 5") {
    fib(5) shouldEqual 5
  }

  test("Array(1, 3, 5, 7) is sorted using isSorted") {
    isSorted(Array(1, 3, 5, 7), (a: Int, b: Int) => a < b) shouldEqual true
  }

  test("Array(7, 5, 1, 3) is not sorted using isSorted") {
    isSorted(Array(7, 5, 1, 3), (a: Int, b: Int) => a > b) shouldEqual false
  }

  test("Array('Scala', 'Exercises') is not sorted using isSorted") {
    isSorted(
      Array("Scala", "Exercises"),
      (x: String, y: String) => x.length < y.length
    ) shouldEqual true
  }

  def f1(a: Int, b: Int): Int = a + b
  def g1(a: Int)(b: Int): Int = a + b

  test("curry(f1)(1)(1) == f1(1, 1)") {
    curry(f1)(1)(1) shouldEqual f1(1, 1)
  }

  test("curry(f1)(1)(1) == g1(1)(1)") {
    curry(f1)(1)(1) shouldEqual g1(1)(1)
  }

  def f3(b: Int): Int = b / 2
  def g3(a: Int): Int = a + 2

  test("compose(f3, g3)(0) == compose(g3, f3)(0) == false") {
    compose(f3, g3)(0) == compose(g3, f3)(0) shouldEqual false
  }

  test("compose(f3, g3)(2) == 2") {
    compose(f3, g3)(2) shouldEqual 2
  }

  test("compose(g3, f3)(2) == 3") {
    compose(g3, f3)(2) shouldEqual 3
  }

}
