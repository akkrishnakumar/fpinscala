package chapter3

import Ex3_1.x
import Ex3_2.tail
import Ex3_3.setHead
import Ex3_4.drop
import Ex3_5.dropWhile
import Ex3_6.init
import Ex3_7.foldRight
import Ex3_9.length
import Ex3_10.foldLeft
import Ex3_11.sum3
import Ex3_11.product3
import Ex3_11.length2
import scala.collection.mutable.ListBuffer

trait Chapter3

object Chapter3 {

  def apply() = {
    assert(x == 3)

    assert(tail(List(1, 2, 3)) == List(2, 3))
    assert(tail(List(1)) == Nil)

    assert(setHead(List(1, 2, 3), 3) == List(3, 2, 3))
    assert(setHead(List("a", "b"), "c") == List("c", "b"))

    assert(drop(List(1, 2, 3), 1) == List(2, 3))
    assert(drop(List(1, 2, 3), 0) == List(1, 2, 3))
    assert(drop(List("a", "b"), 2) == Nil)
    assert(drop(List(1, 2), 3) == Nil)
    assert(drop(Nil, 1) == Nil)

    assert(dropWhile(List(1, 2, 3), (x: Int) => x < 2) == List(2, 3))
    assert(dropWhile(List(1, 2, 3), (x: Int) => x > 2) == List(1, 2, 3))
    assert(dropWhile(List(1, 2, 3), (x: Int) => x > 0) == Nil)
    assert(dropWhile(Nil, (x: Int) => x > 0) == Nil)

    assert(init(List(1, 2, 3)) == List(1, 2))
    assert(init(List(1)) == Nil)

    assert(foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y) == 6)
    assert(1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y) == 6)
    assert(1 + 2 + foldRight(Cons(3, Nil), 0)((x, y) => x + y) == 6)
    assert(1 + 2 + 3 + foldRight(Nil: List[Int], 0)((x, y) => x + y) == 6)
    assert(1 + 2 + 3 + 0 == 6)

    // Exercise 3.8
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == Cons(1, Cons(2, Cons(3, Nil))))

    def l = List(1, 2, 3, 4, 5)
    assert(length(l) == 5)

    // Exercise 3.10, 3.11
    def listInts    = List(1, 2, 3, 4, 5)
    def listDoubles = List(1.0, 2.0, 3.0)
    assert(sum3(listInts) == 15)
    assert(product3(listDoubles) == 6.0)
    assert(length2(listInts) == 5)

    

  }
}
