package chapter2

object Ex2_1 {
  def fib(n: Int): Int = {
    if (n < 1) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }
}
