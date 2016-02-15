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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng1) = rng.nextInt
    (if (v == Int.MaxValue) 0 else Math.abs(v), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => if (i == Int.MaxValue) 0 else i.toDouble / Int.MaxValue)(rng)
  }

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

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.reverse.foldLeft(unit(List.empty[A]))((rb, ra) => map2(ra, rb)(_ :: _))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State(s => (f(a), s)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.reverse.foldLeft(unit[S,List[A]](Nil))((sb, sa) => sa.map2(sb)(_ :: _))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def update = (i: Input) => (s: Machine) => (i, s) match {
    case (Coin, Machine(true, candies, coins)) =>
      if (candies > 0) Machine(false, candies, coins - 1)
      else s
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    case _ => s
  }

  def simulateMachineVerbose(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val m = modify[Machine] _  // (Machine => Machine) => State[Machine, Unit]
    val mu = m.compose(update) // Input => State[Machine, Unit]
    val si = inputs.map(mu)    // List[State[Machine, Unit]]
    val s = sequence(si)       // State[Machine, List[Unit]]
    val s1 = s.flatMap(_ => get) //flatMap calls run passing s's state
    s1.map(s => (s.coins, s.candies))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
