// Advanced Programming 2015
// IT University of Copenhagen
//
// Group 32
// SDEH
// AHAQ
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

//noinspection OptionEqualsSome,EmptyParenMethodAccessedAsParameterless
object Main extends App {
  // this is how we do simple interactive testing

  val l1: Stream[Int] = Empty
  val l2: Stream[Int] = empty

  val l3: Stream[Int] = cons(1, cons(2, cons(3, empty)))

  //println (l1.headOption)
  //println (l2.headOption)
  //println (l3.headOption)

  //Exercise 2 test
  assert(l3.toList == List(1, 2, 3))

  //Exercise 3 test
  assert(l3.take(2).toList == List(1, 2))
  assert(l3.drop(1).toList == List(2, 3))
  assert(l3.drop(2).toList == List(3))
  assert(naturals.take(1000000000).drop(41).take(10).toList == List(41, 42, 43, 44, 45, 46, 47, 48, 49, 50)) //drops the first 41 0..40, takes 10 (41..50)
  //It terminates quickly and without any out of memory exceptions because it only actually does compute for the first 50 elements.

  //Exercise 4 test
  assert(naturals.takeWhile(_ < 1000000000).drop(100).take(50).toList == from(100).take(50).toList)
  //It terminates quickly and without any out of memory exceptions because it only actually does compute for the first 150 elements.

  //Exercise 5 test
  assert(!naturals.forAll(_ < 0))
  //naturals.forAll (_ >=0) //This call is uncommented since it (correctly) throws a StackOverflowError
  //Because they can cause an out of memory exception to be thrown if the predicate computes for a long time (~infinite)

  //Exercise 6 test
  assert(naturals.takeWhile1(_ < 1000000000).drop(100).take(50).toList == from(100).take(50).toList)

  //Exercise 7 test
  assert(naturals.headOption1() == Some(0))
  assert(from(10).headOption1() == Some(10))
  assert(to(100).headOption1() == Some(0))
  assert(to(100).take(50).drop(10).headOption1() == Some(10))

  //Exercise 8.1 test
  naturals.map(_ * 2).drop(30).take(50).toList
  //Exercise 8.2 test
  naturals.drop(42).filter(_ % 2 == 0).take(30).toList
  //Exercise 8.3 test
  naturals.append(naturals)
  naturals.take(10).append(naturals).take(20).toList
  //Exercise 8.4 test
  naturals.flatMap(to _).take(100).toList

  //Exercise 10 test
  assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

  //Exercise 11 test
  assert(unfold(0)(s => Some(s, s + 1)).take(50).toList == naturals.take(50).toList)

  //Exercise 12 test
  assert(from(1).take(1000000000).drop(41).take(10).toList == from1(1).take(1000000000).drop(41).take(10).toList)
  assert(fibs1.take(100).toList == fibs.take(100).toList)

  //Exercise 13 test
  assert(naturals.map(_ * 2).drop(30).take(50).toList == naturals.mapByUnfold(_ * 2).drop(30).take(50).toList)
  assert(naturals.drop(5).takeByUnfold(5).toList == naturals.drop(5).take(5).toList)
  assert(naturals.takeWhileByUnfold(_ < 1000000000).drop(100).take(50).toList == naturals.takeWhile(_ < 1000000000).drop(100).take(50).toList)
  naturals.zipWithByUnfold(naturals)((a, b) => a + b).take(10).toList
  println(naturals.zipAllByUnfold(naturals)((a, b) => a + b).take(10).toList)

  //Exercise 14 test
  // naturals.startsWith (naturals) <-- Will never terminate because they are both infinite and the same so the
  // comparison should continues forever on and on.
  assert(naturals.startsWith(naturals.take(10)))
  assert(!naturals.startsWith(naturals.drop(10).take(10)))
  assert(!naturals.startsWith(fibs)) // Terminates even though both are infinite since fib is not contained.
  println(naturals.take(10).tails.toList.flatMap(s => s.toList)) //Todo: Figure out what is mean by this test case:  write an expression computing the first ten of 10-element prefixes of naturals.tails.

}
