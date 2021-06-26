package chapter2

import Ex2_1.fib
import Ex2_2.isSorted
import Ex2_3.curry
import Ex2_3.uncurry
import Ex2_3.compose

trait Chapter2

object Chapter2 {

  def apply() = {
    // Chapter 2
    assert(fib(5) == 5)

    assert(isSorted(Array(1, 3, 5, 7), (a: Int, b: Int) => a < b) == true)
    assert(isSorted(Array(7, 5, 1, 3), (a: Int, b: Int) => a > b) == false)
    assert(
      isSorted(Array("Scala", "Exercises"), (x: String, y: String) => x.length < y.length) == true
    )

    def f1(a: Int, b: Int): Int = a + b
    def g1(a: Int)(b: Int): Int = a + b
    assert(curry(f1)(1)(1) == f1(1, 1))
    assert(curry(f1)(1)(1) == g1(1)(1))

    def f2(a: Int, b: Int): Int = a + b
    def g2(a: Int)(b: Int): Int = a + b
    assert(uncurry(g2)(1, 1) == g2(1)(1))
    assert(uncurry(g2)(1, 1) == f2(1, 1))

    def f3(b: Int): Int = b / 2
    def g3(a: Int): Int = a + 2
    assert((compose(f3, g3)(0) == compose(g3, f3)(0)) == false)
    assert(compose(f3, g3)(2) == 2)
    assert(compose(g3, f3)(2) == 3)

  }

}
