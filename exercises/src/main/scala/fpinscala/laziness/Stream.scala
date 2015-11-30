package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }
  def toList: List[A] = foldRight(List.empty[A])((a,b) => a :: b)

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  def append[B>:A](b: => Stream[B]): Stream[B] = foldRight(b)(cons(_, _))
  def flatMap[B>:A](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case Empty => None
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)){
    case (m, Cons(h,t)) if m > 0 => Some(h(), (m - 1, t()))
    case _ => None
  }

  //  take, takeWhile, zipWith (as in chapter 3), and zipAll
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = //Stream.cons(1, ones)
    unfold(1)(x=>Some(1,1))
  def constant[A](a: A): Stream[A] = //cons(a, constant(a))
    unfold(a)(x => Some((x,x)))
  def from(n: Int): Stream[Int] = //cons(n, from(n+1))
    unfold(n)(a => Some(a, a+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

}