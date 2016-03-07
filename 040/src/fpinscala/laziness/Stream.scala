// Advanced Programming 2015
// IT University of Copenhagen
//
// Group 32
// SDEH
// AHAQ
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  //Exercise 2
  def toList :List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  //Exercise 3
  def take (n :Int) :Stream[A] = {
    if (n > 0)
      this match {
        case Empty => empty
        case Cons(h, t) => cons(h(), t().take(n-1))
      }
    else
      Empty
  }

  def drop (n :Int) :Stream[A] = {
    if (n > 0)
      this match {
        case Empty => empty
        case Cons(h,t) => t().drop(n-1)
      }
    else
      this
  }

  //Exercise 4
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  }

  //Exercise 5
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  //Exercise 6
  def takeWhile1(p: A => Boolean): Stream[A] = foldRight(empty:Stream[A]) ((cur, acc) => this match {
    case Empty => acc
    case _ => if (p(cur)) cons(cur, acc) else acc
  })

  //Exercise 7
  def headOption1(): Option[A] = foldRight(None:Option[A]) ((cur, acc) => this match {
    case Empty => acc
    case _ => Some(cur)
  })

  //Exercise 8.1
  def map[B] (f: (A => B)): Stream[B] = this match {
    case Empty => empty
    case Cons(h, t) => cons(f(h()), t().map(f))
  }

  //Exercise 8.2
  def filter (f: A=> Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (f(h())) cons(h(), t().filter(f)) else t().filter(f)
  }

  //Exercise 8.3
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  //Exercise 8.4
  def flatMap[B] (f: A=>Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => (f(h)).append(t))

  //Exercise 9
  //Explanation:
  //It is efficient for a stream because basically the only items that are filtered are the ones before and up until the
  //matching element. In a list the corresponding would be very

  //Exercise 13
  def mapByUnfold[B] (f: (A => B)): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeByUnfold(n :Int) :Stream[A] = unfold((n,this)) (s => if (s._1>0) s._2 match {
    case Cons(h, t) => Some(h(), (s._1-1,t()))
    case Empty => None
  } else None)

  def takeWhileByUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
    case Empty => None
  }

  //TODO Not sure if this implementation is correct?
  def zipWithByUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
  unfold((this,s2)) (s => s._1 match {
    case Cons(h, t) => s._2 match {
      case Cons(h2, t2) => Some(f(h(),h2()), (t(), t2()))
      case Empty => None
    }
    case Empty => None
  })

  def zipAllByUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
  unfold((this,s2)) (s => s match {
    case (Cons(h, t), Cons(h1, t1)) => Some((h(), h1()), t(), t1())
    case (Cons(h, t), Empty) => Some((h(), None), t())
    case (Empty, Cons(h, t)) => Some(None, h())
    case _ => None
  })

  //Exercise 14
  def startsWith[B>:A](that: Stream[B]): Boolean = {
    def startsWithHelp(fst: Stream[A], snd: Stream[B]): Boolean = fst match {
      case Cons(h, t) => snd match {
        case Cons(h2, t2) => if (h()==h2()) startsWithHelp(t(), t2()) else false
        case Empty => true
      }
      case Empty => snd == empty
    }

    startsWithHelp(this, that)
  }

  //Exercise 15
  def tails: Stream[Stream[A]] = this match {
    case Cons(h, t) => cons(this, t().tails)
    case Empty => empty
  }

  def headOption () :Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def tail :Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
    case Empty => z
    case Cons (h,t) => f (h(), t().foldRight (z) (f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
    case Empty => z
    case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists (p : A => Boolean) :Boolean = this match {
    case Empty => false
    case Cons (h,t) => p(h()) || t().exists (p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics
    // of ||)
  }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Note 1: ":_*" tells Scala to treat a list as multiple params
  // Note 2: pattern matching with :: does not seem to work with Seq, so we
  //         use a generic function API of Seq

  //Exercise 1
  def to(n: Int): Stream[Int] = {
    def help(cur: Int, max: Int): Stream[Int] = {
      if (cur <= max)
        cons(cur, help(cur + 1, max))
      else
        empty
    }
    help(0, n)
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def naturals = from(0)

  //Exercise 10
  def fibs: Stream[Int] = {
    def f(fst: Int, snd: Int): Stream[Int] = cons(fst, f(snd, fst + snd))
    f(0, 1)
  }

  //Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).fold(empty: Stream[A])({ case (a, s) => cons(a, unfold(s)(f)) })

  //Exercise 12
  def from1(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
  def fibs1: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2,s._1+s._2)))
}
// vim:tw=0:cc=80:nowrap
