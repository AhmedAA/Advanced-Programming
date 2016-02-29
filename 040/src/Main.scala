// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object Main extends App {
  // this is how we do simple interactive testing

  val l1 :Stream[Int] = Empty
  val l2 :Stream[Int] = empty

  val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

  println (l1.headOption)
  println (l2.headOption)
  println (l3.headOption)

  //Exercise 1
  def to (n :Int) :Stream[Int] = {
    def help (cur: Int, max: Int): Stream[Int] = {
      if (cur<=max)
        cons(cur, help(cur+1, max))
      else
        empty
    }
    help(0,n)
  }

  def from (n :Int) :Stream[Int] = if (n>=0) cons(n, from(n+1)) else empty

  //Exercise 2 test
  assert(l3.toList == List(1,2,3))

  //Exercise 3 test
  assert(l3.take(2).toList == List(1,2))
  assert(l3.drop(1).toList == List(2,3))
  assert(l3.drop(2).toList == List(3))
  assert(from(0).take(1000000000).drop(41).take(10).toList == List(41, 42, 43, 44, 45, 46, 47, 48, 49, 50)) //drops the first 41 0..40, takes 10 (41..50)
  //It terminates quickly and without any out of memory exceptions because it only actually does compute for the first 50 elements.

  //Exercise 4 test
  assert(from(0).takeWhile(_<1000000000).drop(100).take(50).toList == from(100).take(50).toList)
  //It terminates quickly and without any out of memory exceptions because it only actually does compute for the first 150 elements.

  //Exercise 5 test
  assert(!from(0).forAll(_ < 0))
  //from(0).forAll (_ >=0) //This call is uncommented since it (correctly) throws a StackOverflowError
  //Because they can cause an out of memory exception to be thrown if the predicate computes for a long time (~infinite)



}