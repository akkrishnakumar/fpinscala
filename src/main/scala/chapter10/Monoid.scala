package chapter10

trait Monoid[A] {
  def op(a: A, b: A): A
  def identity: A
}
