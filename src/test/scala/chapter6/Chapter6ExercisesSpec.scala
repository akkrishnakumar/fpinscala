package chapter6

import utils.BaseSpec
import Chapter6._

class Chapter6ExercisesSpec extends BaseSpec {

  test("Ex 6.1 - Generate random integers from 0 to Int.maxValue (inclusive)") {
    val rng             = SimpleRNG(47)
    val (result1, rng1) = nonNegativeInt(rng)
    val result2         = nonNegativeInt(rng1)._1

    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

  test("Ex 6.2 - a function 'double' to generate a double between 0 - 1, excluding 1") {

    val rng             = SimpleRNG(47)
    val (double1, rng1) = double(rng)
    val double2         = double(rng1)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  ignore("Ex. 6.3.1 - a function 'intDouble' to generate a (int,double)") {
    // Implemented method - intDouble
  }

  ignore("Ex. 6.3.2 - a function 'doubleInt' to generate a (double, int)") {
    // Implemented method - doubleInt
  }

  ignore("Ex. 6.3.3 - a function 'double3' to generate a (double,double, double)") {
    // Implemented method - double3
  }

  test("Ex. 6.4 - a function 'ints' to generate a list of random integers") {
    val (list1, rng1) = ints(5)(SimpleRNG(47))
    val list2         = ints(5)(rng1)._1

    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  test("Ex. 6.5 - Implement double in a more elegant way using map") {
    val rng             = SimpleRNG(47)
    val (double1, rng2) = doubleRefined(rng)
    val double2         = double(rng2)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  ignore("Ex. 6.6 - Implement map2 to combine two actions and then apply map function to it.") {
    // Implemented method - map2
  }

  ignore("Ex 6.7 - Implement sequence method to convert List[Rand[A]] -> Rand[List[A]]") {
    // Implemented method - sequence
  }

  test("Ex 6.8 - Implement flatMap method to implement nonNegativeLessThan method") {
    val (result1, rng1) = nonNegativeLessThan(10)(SimpleRNG(47))
    val result2         = nonNegativeLessThan(10)(rng1)._1

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }

  ignore("Ex 6.9.1 - Implement map in terms of flatMap for Rand") {
    // Implemented method - mapViaFlatMap
  }

  ignore("Ex 6.9.2 - Implement map2 in terms of flatMap for Rand") {
    // Implemented method - map2ViaFlatMap
  }

  ignore("Ex 6.10.1 - Implement unit method for State") {
    // Implemented method - unit
  }

  ignore("Ex 6.10.2 - Implement map method for State") {
    // Implemented method - map
  }

  ignore("Ex 6.10.3 - Implement map2 method for State") {
    // Implemented method - map2
  }

  ignore("Ex 6.10.4 - Implement flatMap method for State") {
    // Implemented method - flatMap
  }

  ignore("Ex 6.10.5 - Implement sequence method for State") {
    // Implemented method - sequence
  }

  // This is just an exercise. Tests shouldn't contain multiple assertions
  test("Ex 6.11 - Implement a candy vending machine") {
    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val machine1 = Machine(true, 1, 0)
    simulateMachine(inputCoin).run(machine1)._2.locked shouldBe false

    val machine2 = Machine(false, 1, 1)
    val m2Result = simulateMachine(inputTurn).run(machine2)
    m2Result._2.locked shouldBe true
    m2Result._2.candies shouldBe 0

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    simulateMachine(inputTurn).run(machine1)._2.locked shouldBe machine1.locked
    simulateMachine(inputCoin).run(machine2)._2.locked shouldBe machine2.locked

    // A machine that’s out of candy ignores all inputs.
    val machine3 = Machine(true, 0, 1)
    simulateMachine(inputTurn).run(machine3)._2.locked shouldBe machine3.locked
    simulateMachine(inputCoin).run(machine3)._2.locked shouldBe machine3.locked

  }

}
