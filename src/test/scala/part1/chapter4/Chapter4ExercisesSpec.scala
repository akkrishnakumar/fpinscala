package chapter4

import utils.BaseSpec
import Chapter4._
import scala.util.Try
import scala.util.Success

class Chapter4ExercisesSpec extends BaseSpec {

  def lookupByName(name: String): Option[Employee] =
    name match {
      case "Joe"   => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Some(Employee("Mary", "IT", None))
      case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
      case _       => None
    }

  def lookupByNameViaEither(name: String): Either[String, Employee] =
    name match {
      case "Joe"   => Right(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Right(Employee("Mary", "IT", None))
      case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
      case _       => Left("Employee not found")
    }

  def getManagerViaEither(employee: Either[String, Employee]): Either[String, String] =
    employee.flatMap(e =>
      e.manager match {
        case Some(e) => Right(e)
        case _       => Left("Manager not found")
      }
    )

  test("Ex 4.1.1 - Implement map method for Option") {
    getDepartment(lookupByName("Joe")) shouldBe Some("Finances")
    getDepartment(lookupByName("Mary")) shouldBe Some("IT")
    getDepartment(lookupByName("Foo")) shouldBe None
  }

  test("Ex 4.1.2 - Implement flatMap method for Option") {
    getManager(lookupByName("Joe")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")) shouldBe None
    getManager(lookupByName("Foo")) shouldBe None
  }

  ignore("Ex 4.1.3 - Implement getOrElse method for Option") {
    // Implemented method
  }

  test("Ex 4.1.4 - Implement orElse method for Option") {
    getManager(lookupByName("Joe")).orElse(Some("Mr. CEO")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")
    getManager(lookupByName("Foo")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")
  }

  test("Ex 4.1.5 - Implement filter method for Option") {
    lookupByName("Joe")
      .filter(_.department != "IT") shouldBe Some(Employee("Joe", "Finances", Some("Julie")))
    lookupByName("Mary").filter(_.department != "IT") shouldBe None
    lookupByName("Foo").filter(_.department != "IT") shouldBe None
  }

  test("Ex 4.2 - Implement variance method") {
    val set: Seq[Double] = Seq(6, 7, 3, 9, 10, 15)
    variance(set) shouldBe Some(13.888888888888891)
  }

  test("Ex 4.3 - Implement map2 method to lifts the result of a binary function") {
    // Implemented method
  }

  test("Ex 4.4 - Implement sequence method which converts list of option to option of list") {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(List(Some(1), Some(2), None)) shouldBe None
  }

  test("Ex 4.5 - Implement traverse without using map for sequence") {
    val list1 = List("1", "2", "3")
    val list2 = List("I", "II", "III", "IV")

    def parseInt(a: String): Option[Int] =
      Try(a.toInt) match {
        case Success(r) => Some(r)
        case _          => None
      }

    traverse(list1)(i => parseInt(i)) shouldBe Some(List(1, 2, 3))
    traverse(list2)(i => parseInt(i)) shouldBe None
  }

  test("Ex 4.6.1 - Implement map method for Either") {
    getDepartment(lookupByNameViaEither("Joe")) shouldBe Right("Finances")
    getDepartment(lookupByNameViaEither("Mary")) shouldBe Right("IT")
    getDepartment(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")
  }

  test("Ex 4.6.2 - Implement flatMap method for Either") {
    getManagerViaEither(lookupByNameViaEither("Joe")) shouldBe Right("Julie")
    getManagerViaEither(lookupByNameViaEither("Mary")) shouldBe Left("Manager not found")
    getManagerViaEither(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")
  }

  test("Ex 4.6.3 - Implement orElse method for Either") {
    getManagerViaEither(lookupByNameViaEither("Joe"))
      .orElse(Right("Mr. CEO")) shouldBe Right("Julie")
    getManagerViaEither(lookupByNameViaEither("Mary"))
      .orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")
    getManagerViaEither(lookupByNameViaEither("Foo"))
      .orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")
  }

  test("Ex 4.6.4 - Implement map2 method for Either") {
    def employeesShareDepartment(employeeA: Employee, employeeB: Employee) =
      employeeA.department == employeeB.department

    lookupByNameViaEither("Joe")
      .map2(lookupByNameViaEither("Mary"))(employeesShareDepartment) shouldBe Right(false)
    lookupByNameViaEither("Mary")
      .map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe Right(true)
    lookupByNameViaEither("Foo")
      .map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe Left(
      "Employee not found"
    )
  }

  test("Ex 4.7.1 - Implement traverse method for Either") {
    val employees              = List("Joe", "Mary")
    val employeesAndOutsources = employees :+ "Foo"

    Either.traverse(employees)(lookupByNameViaEither) shouldBe Right(
      List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None))
    )
    Either.traverse(employeesAndOutsources)(lookupByNameViaEither) shouldBe Left(
      "Employee not found"
    )
  }

  test("Ex 4.7.2 - Implement sequence method for Either") {
    val employees              = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
    val employeesAndOutsources = employees :+ lookupByNameViaEither("Foo")

    Either.sequence(employees) shouldBe Right(
      List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None))
    )
    Either.sequence(employeesAndOutsources) shouldBe Left("Employee not found")
  }

}
