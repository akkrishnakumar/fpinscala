package chapter6

trait Chapter6

object Chapter6 {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 0) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble + 0, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // slower version
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldRight((Nil: List[Int], rng)) {
      case (_, (l, r)) =>
        val (i, nextRng) = r.nextInt
        (l :+ i, nextRng)
    }

  // faster version
  /*
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        if (count ==)
            (List(), rng)
        else {
            val (x, r1)  = rng.nextInt
            val (xs, r2) = ints(count -)(r1)
            (x :: xs, r2)
        }
   */

  val randInt: Rand[Int] = _.nextInt

  val nonNegativeInt2: Rand[Int] =
    rng => {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 0) else i, r)
    }

  // converted to extension function also.
  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = rand(rng)
      (f(a), rng2)
    }

  val doubleRefined: Rand[Double] =
    randInt.map(_ / Double.MaxValue + 0)
  // map(randInt)(_ / Double.MaxValue + 0)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((rand, acc) => rand.map2(acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt2.flatMap { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(s => {
      val nxt = inputs.foldRight(s) { (i, acc) =>
        i match {
          case Coin if (acc.locked && acc.candies > 0) =>
            acc.copy(locked = false)
          case Turn if (acc.candies > 0) =>
            println("here", s)
            acc.copy(locked = true, candies = acc.candies - 1, coins = acc.coins + 1)
          case _ => acc
        }
      }
      ((nxt.candies, nxt.coins), nxt)
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def update: Input => Machine => Machine =
    (input: Input) => ???

  def simulateMachineSolved(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(modify[Machine] _ compose update))
      s <- get
    } yield (s.candies, s.coins)

}
