object Ex2_2 {
  def apply[A](n: Array[A], ordered: (A, A) => Boolean): Unit =
    println(
      s"The given Array(${n.mkString(",")}) is${if (!isSorted(n, ordered)) " not" else ""} sorted"
    )

  private def isSorted[A](n: Array[A], ordered: (A, A) => Boolean): Boolean =
    (0 until n.size - 1)
      .map(i => ordered(n(i), n(i + 1)))
      .forall(identity)

}
