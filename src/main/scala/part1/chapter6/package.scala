package object chapter6 {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  implicit class RandExt[A](rand: Rand[A]) {

    def map[B](f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = rand(rng)
        (f(a), rng2)
      }

    def map2[B, C](rand2: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng1) = rand(rng)
        val (b, rng2) = rand2(rng1)
        (f(a, b), rng2)
      }

    def flatMap[B](f: A => Rand[B]): Rand[B] =
      rng => {
        val (a, nxtRng) = rand(rng)
        f(a)(nxtRng)
      }

    def mapViaFlatMap[B](f: A => B): Rand[B] =
      rand.flatMap(a => unit(f(a)))

    def map2ViaFlatMap[B, C](rand2: Rand[B])(f: (A, B) => C): Rand[C] =
      rand.flatMap(a => rand2.map(b => f(a, b)))

  }

  type Rand2[+A] = State[RNG, A]

}
