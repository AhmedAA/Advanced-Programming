package practice

import adpro.FingerTree.{Deep, Empty, FingerTree, Single}
import adpro.monads.Monad
import adpro.monoids.Monoid

import scala.runtime.RichInt

/**
  * Created by seb on 5/26/2016.
  */
object PraticeStuff extends App {
  //First
  val l1 = List(1,2,3,4,5,6,7,8,9,10)
  def plusMinus(in:List[Int]): Int =
    in.foldLeft((1,true)) ((tuple,cur) => if (tuple._2) (tuple._1-cur,false) else (tuple._1+cur,true))._1

  println(s"6 = ${plusMinus(l1)}")

  //Second
  val l2 = List('a','z','D','X','c','e','y')
  val biggestCharMonoid = new Monoid[Char] {
    def op(a1: Char, a2: Char) = if (a1>a2) a1 else a2
    val zero = 'a'
  }
  def biggestChar(in:List[Char]):Char = in.foldLeft(biggestCharMonoid.zero)((acc,cur) => biggestCharMonoid.op(acc,cur))

  println(s"z = ${biggestChar(l2)}")

  //Third
  val a1 = IndexedSeq(3,2,1,4,7,8,1,3)

  def indexedSeqMonad: Monad[IndexedSeq] = new Monad[IndexedSeq] {
    override def unit[A](a: => A): IndexedSeq[A] = IndexedSeq(a)
    override def flatMap[A, B](ma: IndexedSeq[A])(f: (A) => IndexedSeq[B]): IndexedSeq[B] = ma.flatMap(a => f(a))
  }

  case class indexedTools[A](in:IndexedSeq[A]) {
    def repeatThreeTimes :IndexedSeq[A] = indexedSeqMonad.flatMap(in)(a => IndexedSeq(a,a,a))

    def toLeftTree :FingerTree[A] = in.foldLeft(Empty():FingerTree[A])((acc,cur) => acc.addL(cur))
  }
  val repeated = indexedTools(a1).repeatThreeTimes
  println(s"(3,2,1,4) = ${(repeated(0),repeated(3),repeated(6),repeated(9))}")

  //Fourth
  val tree = indexedTools(a1).toLeftTree

  def sort(in: FingerTree[Int]) :FingerTree[Int] = indexedTools(in.toList.sorted.toIndexedSeq).toLeftTree

  println(s"Unsorted: ${tree.toList}")
  println(s"Sorted: ${sort(tree).toList}")
}
