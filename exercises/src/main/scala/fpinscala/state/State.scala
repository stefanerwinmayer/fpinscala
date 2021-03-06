package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rand, rng_) = rng.nextInt
    val nonNegativeRand = Math.abs(if (rand == Int.MinValue) rand + 1 else rand)
    (nonNegativeRand, rng_)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (rand, rng_) = nonNegativeInt(rng)
    val from0toExcluding1 = (rand - 1) / Integer.MAX_VALUE.toDouble
    (from0toExcluding1, rng_)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randInd, rngNext) = rng.nextInt
    val (randDouble, rngLast) = double(rngNext)
    ((randInd, randDouble), rngLast)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((randInt, randDouble), rng_) = intDouble(rng)
    ((randDouble, randInt), rng_)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (rand1, rng1) = double(rng)
    val (rand2, rng2) = double(rng1)
    val (rand3, rng3) = double(rng2)
    ((rand1, rand2, rand3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, rands: List[Int]): (List[Int], RNG) =
      if (count > 0) {
        val (rand, rng_) = rng.nextInt
        loop(count - 1, rng_, rand :: rands)
      } else {
        (rands, rng)
      }

    loop(count, rng, List())
  }

  def doubleFromMap(rng: RNG): RNG => (Double, RNG) = {
    map(nonNegativeInt) { rand =>
      (rand - 1) / Integer.MAX_VALUE.toDouble
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngNext) = ra(rng)
      val (b, rngFinal) = rb(rngNext)
      (f(a, b), rngFinal)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight(List[A](), rng) {
        case (f, (list, rng_)) =>
          val (a, rngNext) = f(rng_)
          (a :: list, rngNext)
      }
    }

  def sequenceFromMap2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) { (f, acc) =>
      map2(f, acc) { (a, list) =>
        a :: list
      }
    }

  def intsFromSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
