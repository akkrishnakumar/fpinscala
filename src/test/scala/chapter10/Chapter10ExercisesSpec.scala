package chapter10

import utils.BaseSpec
import Chapter10._
import org.scalactic.source.Position
import org.scalatest.Tag
import chapter3.Tree
import chapter3.Branch
import chapter3.Leaf

class Chapter10ExercisesSpec extends BaseSpec {

  test("Ex 10.1.1 -  Monoid instance for integer addition") {
    val sum = 1 + 1
    sum shouldEqual intAddition.op(1, 1)
    intAddition.op(10, intAddition.identity) shouldEqual 10
  }

  test("Ex 10.1.2 - Monoid instance for integer multiplication") {
    val product = 2 * 3
    product shouldEqual intMultiplication.op(2, 3)
    intMultiplication.op(10, intMultiplication.identity) shouldEqual 10
  }

  test("Ex 10.1.3 - Monoid instance for boolean Or") {
    val case1 = true || false
    val case2 = false || false

    case1 shouldEqual booleanOr.op(true, false)
    case2 shouldEqual booleanOr.op(false, false)
    booleanOr.op(true, booleanOr.identity) shouldEqual true
    booleanOr.op(false, booleanOr.identity) shouldEqual false
  }

  test("Ex 10.1.4 - Monoid instance for boolean And") {
    val case1 = true && false
    val case2 = true && true

    case1 shouldEqual booleanAnd.op(true, false)
    case2 shouldEqual booleanAnd.op(true, true)
    booleanAnd.op(true, booleanAnd.identity) shouldEqual true
    booleanAnd.op(false, booleanAnd.identity) shouldEqual false
  }

  test("Ex 10.2 - Monoid instance for Option") {
    optionMonoid.op(Some("a"), None) shouldEqual Some("a")
    optionMonoid.op(None, Some("b")) shouldEqual Some("b")
    optionMonoid.op(Some("a"), Some("b")) shouldEqual Some("ab")

    optionMonoid.op(Some("a"), optionMonoid.identity) shouldEqual Some("a")
    optionMonoid.op(None, optionMonoid.identity) shouldEqual None
  }

  test("Ex 10.3 - Monoid instance for EndoFunction") {
    val endoFunc = (x: Int) => x

    val f = endoMonoid.op(endoFunc, endoFunc)

    f(10) shouldEqual endoFunc(10)
  }

  ignore("Ex 10.4 - Property based test for Monoid") {
    // Didn't Implement
  }

  test("Ex 10.5 - Implement foldMap for Monoid") {
    val input    = List(10, 11, 12)
    val expected = "10, 11, 12,"

    foldMap(input, stringSeparatedByCommaMonoid)(_.toString) shouldEqual expected
  }

  ignore("Ex 10.6 - Implement foldLeft and foldRight using foldMap") {
    // Didn't Implement
  }

  test("Ex 10.7 - Implement FoldMap for IndexedSeq") {
    val seq      = IndexedSeq("10", "20", "30", "40", "50")
    val expected = 150

    foldMapV(seq, intAddition)(_.toInt) shouldEqual expected
  }

  ignore("Ex 10.8 - Implement FoldMapV with parallelism") {
    // Didn't Implement
  }

  ignore("Ex 10.9 - Use foldMap to check if given IndexedSeq is ordered") {
    // Didn't Implement
  }

  test("Ex 10.10 - Implement Monoid for WC trait") {
    val firstPart  = Part("a", 1, "b")
    val secondPart = Part("c", 1, "d")

    wcMonoid.op(firstPart, secondPart) shouldEqual Part("a", 3, "d")
    wcMonoid.op(firstPart, Stub("c")) shouldEqual Part("a", 1, "bc")
    wcMonoid.op(Stub("c"), firstPart) shouldEqual Part("ca", 1, "b")
    wcMonoid.op(Stub("c"), Stub("d")) shouldEqual Stub("cd")
    wcMonoid.op(firstPart, wcMonoid.identity) shouldEqual firstPart
  }

  test("Ex 10.11 - count the number of words using WC") {
    val phrase        = "To be or not to be, that is the question"
    val expectedCount = 10

    wordCount(phrase) shouldEqual expectedCount
  }

  test("Ex 10.12.1 - Implement Foldable[List]") {
    foldableList.foldRight(List(1, 2, 3))(0)(_ + _) shouldEqual 6
  }

  ignore("Ex 10.12.2 - Implement Foldable[IndexedSeq]") {
    // Didn't implement
  }

  ignore("Ex 10.12.3 - Implement Foldable[Stream]") {
    // Didn't implement
  }

  test("Ex 10.13 - Implement Foldable[Tree]") {
    val tree1 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val tree2 = Branch(Branch(Leaf("a"), Leaf("b")), Leaf("c"))

    foldableTree.foldMap(tree1)(_.toString())(stringSeparatedByCommaMonoid) shouldEqual "1, 2, 3,"
    foldableTree.foldLeft(tree2)("")((b, a) => s"$a, $b") shouldEqual "c, b, a, "
  }

  test("Ex 10.14 - Implement Foldable[Option]") {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    foldableOption.foldMap(some)(_ + 1)(intAddition) shouldEqual 2
    foldableOption.foldRight(none)(10)((a, b) => a + b) shouldEqual 10
  }

  ignore("Ex 10.15 - Convert a foldable to a list") {
    // Won't implement due to discrepancy
  }

  test("Ex 10.16 - Implement productMonoid") {
    productMonoid(intAddition, intMultiplication).op((2, 2), (3, 3)) shouldEqual (5, 6)
  }

  ignore("Ex 10.17 - Implement functionMonoid") {
    // Implemented functionMonoid`
  }

  test("Ex 10.18 - Implement 'bag' method") {
    bag(Vector("a", "rose", "is", "a", "rose")) shouldEqual Map("a" -> 2, "rose" -> 2, "is" -> 1)
  }

}
