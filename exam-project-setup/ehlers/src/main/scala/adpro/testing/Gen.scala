// Advanced Programming 2015
// SDEH@ITU.DK
// AHAQ@ITU.DK


// 1. Introduction
//
// This file contains exercises 2--9
//
// This file is best worked on by reading exercise descriptions in the
// corresponding PDF file.  Then find the place in the file where the stub for
// the exercise is created and complete it.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).
//
// I compile this file using sbt, but in case you need to debug sbt, the
// following should also work (if you put both scala files in the same
// directory):
//
// $ fsc State.scala Gen.scala
//
// Then you can load Exercise2.scala into the REPL to interactively test your
// work. (Exercise2.scala is not so key, it just has all the useful imports in
// the header, that make working with REPL more effcient for these exercises).
// This can be done by issuing "sbt console" and importing the write object from
// Exercise2 (the class will be loaded automatically by sbt).
//
// The file is similar to the one created by Chiusano and Bjarnasson, but it
// has small differences, since I created it myself working through the
// chapter.

package adpro.testing

import adpro.state._
import adpro.state.RNG._

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class GenAdPro[A] (sample :State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed :Long):Stream[A] =
    GenAdPro.state2stream(this.sample) (RNG.Simple(seed))
  def toStream (rng :RNG):Stream[A] =
    GenAdPro.state2stream(this.sample) (rng)

  // the book uses Stream.unfold, but apparently the standard library lacks this method

  // Exercise 4 (Ex 8.5 second part)
  //
  // Hint: The standard library has the following useful function (List
  // companion object):
  //
  // def fill[A](n: Int)(elem: ⇒ A): List[A]
  //
  // It is of course possible to implement a solution without it, but the
  // result is ugly (you need to replicate the behavior of fill inside
  // listOfN). Then note that State has a method sequence which allows to take
  // a list of automata and execute their transitions as a sequence, feeding
  // the output state of one as an input to the next.  This can be used to
  // execute a series of consecutive generations, passing the RNG state around.

  def listOfN (n :Int) :GenAdPro[List[A]] = GenAdPro(State.sequence(List.fill(n)(this.sample)))

  // Exercise 5 (Ex. 8.6 [Chiusano, Bjarnasson 2015])
  //
  // So this is a solution that is ignoring the nice API that we developed.
  // It builds the result from ground up.

  def flatMap[B] (f: A => GenAdPro[B]) :GenAdPro[B] = GenAdPro(sample.flatMap(a => f(a).sample))


  // It would be convenient to also have map (uncomment once you have unit and flatMap)

  def map[B] (f : A => B) :GenAdPro[B] = this.flatMap (a => GenAdPro.unit[B] (f(a)))

  // Exercise 6 (Second part of Ex. 8.6)

  def listOfN(size: GenAdPro[Int]): GenAdPro[List[A]] = size.flatMap(x => this.listOfN(x))

  // Exercise 7 (Ex. 8.7; I implemented it as a method, the book asks for a
  // function, the difference is minor; you may want to have both for
  // convenience)
  //
  // Hint: we already have a generator that emulates tossing a coin. Which one
  // is it? Use flatMap with it.

  def union (that :GenAdPro[A]) :GenAdPro[A] = GenAdPro.boolean.flatMap(b => if (b) this else that)

  // Exercise 8 continues in the bottom of the file (in the companion object)
}

object GenAdPro {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => n #:: state2stream (s) (s1) }

  // A generator for Integer instances

  def anyInteger :GenAdPro[Int] = GenAdPro(State(_.nextInt))

  // Exercise 2 (Ex. 8.4)
  //
  // Hint: Before solving the exercise study the type \lstinline{Gen} in
  // \texttt{Gen.scala}. Then, think how to convert a random integer to a
  // random integer in a range.  Then recall that we are already using
  // generators that are wrapped in \texttt{State} and the state has a
  // \lstinline{map} function.

  def choose (start :Int, stopExclusive :Int) :GenAdPro[Int] =
    GenAdPro(State(RNG.nonNegativeInt).map(n => n + start % (stopExclusive-start) ))

  // Exercise 3 (Exercise 8.5, part one)
  //
  // Hint: The \lstinline{State} trait already had \lstinline{unit}
  // implemented.

  def unit[A] (a : =>A) :GenAdPro[A] = GenAdPro(State.unit(a))

  // Hint: How do you convert a random integer number to a random Boolean?
  // Alternatively: do we already have a random generator for booleans? Could
  // we wrap it in.

  def boolean :GenAdPro[Boolean] = GenAdPro(State(RNG.boolean))


  // Hint: Recall from Exercise1.scala that we already implemented a random
  // number generator for doubles.

  def double :GenAdPro[Double] = GenAdPro(State(RNG.double))



  // (Exercise 4 is found in the Gen class above)

  // Exercise 8 (Ex. 8.8 in Chiusano and Bjarnasson)
  //
  // Hint: Recall that we already have a generator of random double numbers
  // from range (0;1); See Exercise 3. First translate weights to
  // probabilities. Then use our generator of doubles to simulate an unfair
  // coin with flatMap.

  def weighted[A](g1: (GenAdPro[A],Double), g2: (GenAdPro[A],Double)): GenAdPro[A] = {
    val g1weight = g1._2.abs / (g1._2.abs + g2._2.abs)
    GenAdPro(State(RNG.double).flatMap(d => if (d < g1weight) g1._1.sample else g2._1.sample))
  }
  //
  // Nice test idea for the above: create 1.0:2.0 boolean generator, translate
  // to stream, and try longer and longer prefixes to see if the law of big
  // numbers works
  //
  // (Exercise 9 is found below in class Prop)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object PropAdPro {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result { def isFalsified: Boolean }
  case object Passed extends Result { def isFalsified = false }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result { def isFalsified = false }

  def forAll[A](as: GenAdPro[A])(f: A => Boolean): PropAdPro = PropAdPro {
    (n,rng) => as.toStream(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import PropAdPro._

case class PropAdPro(run :(TestCases,RNG) => Result) {

  // (Exercise 9)

  def && (that :PropAdPro) :PropAdPro = PropAdPro {
    (n,rng) => run(n,rng) match {
      case Passed | Proved => that.run(n, rng)
      case x => x
    }
  }

  def || (that :PropAdPro) :PropAdPro = PropAdPro {
    (n,rng) => run(n,rng) match {
      case Falsified(msg, _) => that.run(n, rng)
      case x => x
    }
  }
}

// vim:cc=80:foldmethod=indent:foldenable