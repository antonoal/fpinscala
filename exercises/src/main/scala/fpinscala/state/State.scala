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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng1) = rng.nextInt
    (if (v == Int.MaxValue) 0 else Math.abs(v), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => if (i == Int.MaxValue) 0 else i.toDouble / Int.MaxValue)(rng)
  }
//  {
//    val (v, rng1) = nonNegativeInt(rng)
//    (if (v == Int.MaxValue) 0 else v.toDouble / Int.MaxValue, rng1)
//  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (ds, rng1) = (1 to 3).foldLeft((List.empty[Double], rng)) { case ((l, g), _) =>
      val (d, g1) = double(g)
      (d :: l, g1)
    }
    ((ds(0), ds(1), ds(2)), rng1)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill[Rand[Int]](count)((rng1: RNG) => rng1.nextInt))(rng)
  }
//  {
//    (1 to count).foldLeft((List.empty[Int], rng)) { case ((l, g), _) =>
//        val (i, g1) = g.nextInt
//      (i :: l, g1)
//    }
//  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List.empty[A]))((rb, ra) => map2(ra, rb)(_ :: _))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
