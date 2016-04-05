// Advanced Programming
// Ahmed Al Aqtash (ahaq) and Sebastian Ehlers (sdeh), IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecAhaqSdeh extends FlatSpec with Checkers {

  import Stream._

  // --------------------------------------------------------------------------

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  // This returns the head of an unlimited stream, and since we don't crash
  // with a memory exception, we must have asserted that the tail is not run
  it should "return the head of the stream, and make sure execution stops (03)" in {
    def testStream:Stream[Int] = Stream.cons(0, testStream)
    assert(testStream.headOption == Some(0))
  }

  // --------------------------------------------------------------------------

  behavior of "take"

  //TODO Something is wrong with head
  it should "not force any heads nor any tails of the Stream it manipulates & not force (n+1)st head (01+02)" in check {
    Prop.forAll { (n :Int) => {
      var tester = 0
      def testStream:Stream[Int] = {
        tester = tester + 1
        Stream.cons(0, testStream)
      }

      testStream.take(n)
      tester == 1
    }}
  }

  //We put 20 as a hardcoded number inorder to avoid OutOfMemory Exceptions.
  it should "hold that s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("empty" |:
      Prop.forAll { (n :Int) => empty.take(n).take(n) == empty.take(n) } ) &&
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).take(n).take(n).headOption == cons(n,empty).take(n).headOption } ) &&
    ("random" |:
      Prop.forAll {(s :Stream[Int]) => s.take(20).take(20).toList.zip(s.take(20).toList).forall{ case(x,y) => x == y }})
  }

  // --------------------------------------------------------------------------

  behavior of "drop"


  //We put 20 as a hardcoded number inorder to avoid OutOfMemory Exceptions.
  //We have determined that this law will only hold if neither n nor m are negative numbers.
  it should "hold that s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (n :Int, m: Int, s:Stream[Int]) => {
      if (n >= 0 && m >= 0)
        s.drop(n).drop(m).take(20).toList.zip(s.drop(n+m).take(20).toList).forall{ case(x,y) => x == y }
      else
        true
    }}
  }

  //TODO Something is wrong with head
  it should "hold that s.drop(n) does not force any of the dropped elements heads" in check {
    Prop.forAll { (n :Int) => {
      var tester = 0
      def testStream:Stream[Int] = {
        tester = tester + 1
        Stream.cons(0, testStream)
      }

      testStream.drop(50)
      println(tester) //TODO Fails with 51 should actually not happend right????
      tester == 1
    }}
  }

}